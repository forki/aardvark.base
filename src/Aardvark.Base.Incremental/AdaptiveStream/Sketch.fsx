
#I @"..\..\..\bin\Release"
#r @"Aardvark.Base.dll"
#r @"Aardvark.Base.Essentials.dll"
#r @"Aardvark.Base.TypeProviders.dll"
#r @"Aardvark.Base.FSharp.dll"
#r @"Aardvark.Base.Incremental.dll"

open System
open Aardvark.Base
open Aardvark.Base.Incremental

type Event<'a> = { time : DateTime; value : 'a }

type History<'a> = 
    | Update of list<Event<'a>>
    | Finished of list<Event<'a>>

module History =
    let empty = Update []

    let isEmpty (h : History<'a>) =
        match h with
            | Update l -> List.isEmpty l
            | Finished l -> false


    let map (f : 'a -> 'b) (h : History<'a>) : History<'b> =
        match h with
            | Update h      -> h |> List.map (fun e -> { time = e.time; value = f e.value }) |> Update
            | Finished h    -> h |> List.map (fun e -> { time = e.time; value = f e.value }) |> Finished
      
    let choose (f : 'a -> Option<'b>) (h : History<'a>) : History<'b> =
        match h with
            | Update h      -> h |> List.choose (fun e -> match f e.value with | Some v -> Some { time = e.time; value = v } | _ -> None) |> Update
            | Finished h    -> h |> List.choose (fun e -> match f e.value with | Some v -> Some { time = e.time; value = v } | _ -> None) |> Finished
      
    let tryLast (h : History<'a>) : Option<'a> =
        match h with
            | Update h      -> h |> List.tryLast |> Option.map (fun e -> e.value)
            | Finished h    -> h |> List.tryLast |> Option.map (fun e -> e.value)
      
    let toList (h : History<'a>) =
        match h with
            | Update h      -> h |> List.map (fun e -> e.value)
            | Finished h    -> h |> List.map (fun e -> e.value)

    let union (l : History<'a>) (r : History<'a>) =
        let rec intersperse (l : list<Event<'a>>) (r : list<Event<'a>>) =
            match l,r with
                | l'::ls, r'::rs ->
                    if l'.time < r'.time then l'::intersperse ls r
                    else r'::intersperse l rs
                | [], r -> r
                | l, [] -> l

        match l, r with
            | Update l, Update r -> Update(intersperse l r)
            | Update l, Finished r -> Finished(intersperse l r)     
            | Finished l, _ -> Finished l

type IStreamReader<'a> =
    inherit IDisposable
    inherit IAdaptiveObject
    abstract member GetHistory : unit -> History<'a>
    abstract member SubscribeOnEvaluate : (History<'a> -> unit) -> IDisposable

type astream<'a> =
    abstract member GetReader : unit -> IStreamReader<'a>

module AStream =
    open System.Collections.Generic

    [<AbstractClass>]
    type AbstractStreamReader<'a>() =
        inherit AdaptiveObject()

        let callbacks = HashSet<History<'a> -> unit>()

        member x.Callbacks = callbacks

        abstract member Release : unit -> unit
        abstract member ComputeHistory : unit -> History<'a>
        abstract member GetHistory : unit -> History<'a>

        default x.GetHistory() =
            x.EvaluateIfNeeded History.empty (fun () ->
                let h = x.ComputeHistory()

                if not (History.isEmpty h) then
                    for cb in callbacks do cb h

                match h with
                    | Finished _ -> x.Dispose()
                    | _ -> ()

                h
            )

        override x.Finalize() =
            try x.Dispose()
            with _ -> ()

        member x.Dispose() =
            x.Release()

        member x.SubscribeOnEvaluate (cb : History<'a> -> unit) =
            lock x (fun () ->
                if callbacks.Add cb then
                    { new IDisposable with 
                        member __.Dispose() = 
                            lock x (fun () ->
                                callbacks.Remove cb |> ignore 
                            )
                    }
                else
                    { new IDisposable with member __.Dispose() = () }
            )


        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IStreamReader<'a> with
            member x.GetHistory() = x.GetHistory()
            member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb

    let private wrap scope f =
        fun v -> Ag.useScope scope (fun () -> f v)
         
    type MapReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> 'b) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()

        override x.ComputeHistory() =
            source.GetHistory() |> History.map f

    type ChooseReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> Option<'b>) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()

        override x.ComputeHistory() =
            source.GetHistory() |> History.choose f


    type CopyReader<'a>(inputReader : IStreamReader<'a>, dispose : CopyReader<'a> -> unit) as this =
        inherit AbstractStreamReader<'a>()
        
        let mutable history = History.empty

        let emit (d : History<'a>) =
            lock this (fun () ->
                
                history <- History.union history d
            
                if not this.OutOfDate then 
                    // TODO: why is that happening sometimes?

                    // A good case: 
                    //     Suppose the inputReader and this one have been marked and
                    //     a "neighbour" reader (of this one) has not yet been marked.
                    //     In that case the neighbouring reader will not be OutOfDate when
                    //     its emit function is called.
                    //     However it should be on the marking-queue since the input was
                    //     marked and therefore the reader should get "eventually consistent"
                    // Thoughts:
                    //     Since the CopyReader's GetDelta starts with acquiring the lock
                    //     to the inputReader only one CopyReader will be able to call its ComputeDelta
                    //     at a time. Therefore only one can call inputReader.Update() at a time and 
                    //     again therefore only one thread may call emit here.
                    // To conclude I could find one good case and couldn't come up with
                    // a bad one. Nevertheless this is not really a proof of its inexistence (hence the print)
                    Aardvark.Base.Log.warn "[AStreamReader.CopyReader] potentially bad emit with: %A" d
            )

        do inputReader.AddOutput this
        let subscription = inputReader.SubscribeOnEvaluate emit

        override x.GetHistory() =
            lock inputReader (fun () ->
                x.EvaluateIfNeeded History.empty (fun () ->
                    let h = x.ComputeHistory()

                    if not (History.isEmpty h) then
                        for cb in x.Callbacks do cb h

                    match h with
                        | Finished _ -> x.Dispose()
                        | _ -> ()

                    h
                )
            )

        override x.ComputeHistory() =
            inputReader.GetHistory() |> ignore

            let res = history
            history <- History.empty

            res

        override x.Release() =
            inputReader.RemoveOutput x
            subscription.Dispose()
            dispose(x)

    type AdaptiveStream<'a>(newReader : unit -> IStreamReader<'a>) =
        let l = obj()
        let readers = WeakSet<CopyReader<'a>>()

        let mutable inputReader = None
        let getReader() =
            match inputReader with
                | Some r -> r
                | None ->
                    let r = newReader()
                    inputReader <- Some r
                    r


        interface astream<'a> with
            member x.GetReader () =
                lock l (fun () ->
                    let r = getReader()

                    let remove ri =
                        r.RemoveOutput ri
                        readers.Remove ri |> ignore

                        if readers.IsEmpty then
                            r.Dispose()
                            inputReader <- None

                    let reader = new CopyReader<'a>(r, remove)
                    readers.Add reader |> ignore

                    reader :> _
                )

    type AllReader<'a>(source : IStreamReader<'a>) as this =
        inherit ASetReaders.AbstractReader<'a>()
        do source.AddOutput this

        override x.ComputeDelta() =
            source.GetHistory() |> History.toList |> List.map Add

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()
            

    let map (f : 'a -> 'b) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new MapReader<_,_>(scope, stream.GetReader(), f) :> _) :> _

    let choose (f : 'a -> Option<'b>) (stream : astream<'a>) : astream<'b>=
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new ChooseReader<_,_>(scope, stream.GetReader(), f) :> _) :> _

    let latest (stream : astream<'a>) : IMod<Option<'a>> =
        let r = stream.GetReader()
        let mutable current = None
        [r :> IAdaptiveObject] |> Mod.mapCustom (fun () ->
            match r.GetHistory() |> History.tryLast with
                | Some l -> 
                    current <- Some l
                    Some l
                | None ->
                    current
        )

    let all (stream : astream<'a>) : aset<'a> =
        ASet.AdaptiveSet(fun () -> new AllReader<_>(stream.GetReader()) :> _) :> _

    let collect (f : 'a -> astream<'b>) (stream : astream<'a>) : astream<'b> =
        failwith ""

    let union (stream : aset<astream<'a>>) : astream<'a> =
        failwith ""

    let partition (m : IMod<bool>) (stream : astream<'a>) : astream<astream<'a>> =
        failwith ""

    let fold (f : 's -> 'a -> 's) (seed : 's) (stream : astream<'a>) : IMod<'s> =
        failwith ""

    let final (f : 'a -> 'r) (stream : astream<'a>) : IMod<Option<'r>> =
        failwith ""

    let fold2 (final : 's -> 'r) (f : 's -> 'a -> 's) (seed : 's) (stream : astream<'a>) : Either<IMod<'s>, 'r> =
        failwith ""


module Sketch =
    
    let clicks : astream<V2i> = failwith ""
    let sketching = Mod.init true

    type Workflow<'i, 'f> = IMod<Option<'i>> * astream<'f>

    // Workflow<list<_>, PersistentHashSet<_>>
    // Workflow<IMod<list<_>> * IMod<float>, PersistentHashSet<_> * int>
    // Workflow<IMod<list<_>>, PersistentHashSet<_>>



//    let test = //: IMod<Option<list<V2i>>> * astream<PersistentHashSet<V2i>> =
//        seq {
//            while sketching do
//                let mutable poly = []
//                for c in clicks do
//                    poly <- c :: poly
//                    yield poly
//                        
//
//                return PersistentHashSet.ofList !poly
//
//        }


    let test =
        clicks 
            |> AStream.partition sketching
            |> AStream.map (fun s ->
                s |> AStream.fold2 PersistentHashSet.ofList (fun poly p -> p::poly) []
               )

    let final =
        test
            |> AStream.choose (fun e ->
                    match e with
                        | Right final -> Some final
                        | _ -> None
               )
            |> AStream.all

    let current =
        test
            |> AStream.latest
            |> Mod.map (fun e ->
                match e with
                    | Some (Left i) -> Some i
                    | _ -> None
            )



