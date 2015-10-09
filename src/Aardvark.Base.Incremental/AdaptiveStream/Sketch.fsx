
#I @"..\..\..\bin\Release"
#r @"Aardvark.Base.dll"
#r @"Aardvark.Base.Essentials.dll"
#r @"Aardvark.Base.TypeProviders.dll"
#r @"Aardvark.Base.FSharp.dll"
#r @"Aardvark.Base.Incremental.dll"
#r @"FSharp.Quotations.Evaluator.dll"

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open System.Collections.Generic


// =============================================================
// Utilities
// =============================================================

[<CustomComparison; CustomEquality>]
type Event<'a> = { time : int64; value : 'a } with

    override x.GetHashCode() =
        HashCode.Combine(x.time.GetHashCode(), (x.value :> obj).GetHashCode())

    override x.Equals o =
        match o with
            | :? Event<'a> as o -> x.time = o.time && Object.Equals(x.value, o.value)
            | _ -> false

    interface IComparable with
        member x.CompareTo(o) =
            match o with
                | :? Event<'a> as o -> compare x.time o.time
                | _ -> failwithf "cannot compare event to %A" o

module Event =
    let inline time (e : Event<'a>) = e.time

    let inline value (e : Event<'a>) = e.value


    let map (f : 'a -> 'b) (e : Event<'a>) =
        { time = e.time; value = f e.value }

    let collect (f : 'a -> #seq<'b>) (e : Event<'a>) =
        e.value |> f |> Seq.map (fun v -> { time = e.time; value = v })


    let choose (f : 'a -> Option<'b>) (e : Event<'a>) =
        match f e.value with
            | Some v -> Some { time = e.time; value = v }
            | None -> None

        
type History<'a> = { events : list<Event<'a>>; finished : bool }

module History =

    let mutable currentTime = 0L
    let now() =
        System.Threading.Interlocked.Increment &currentTime

    type private EmptyHistoryImpl<'a>() =
        static let empty : History<'a> = { events = []; finished = false }
        static let finished : History<'a> = { events = []; finished = false }
        static member Empty = empty
        static member Finished = finished

    let empty<'a> : History<'a> = EmptyHistoryImpl<'a>.Empty

    let finished<'a> : History<'a> = EmptyHistoryImpl<'a>.Finished

    let inline isFinished (h : History<'a>) = h.finished

    let inline isEmpty (h : History<'a>) = not h.finished && h.events |> List.isEmpty

    let inline toList (h : History<'a>) = h.events

    let toSeq (h : History<'a>) = h.events |> List.toSeq

    let toArray (h : History<'a>) = h.events |> List.toArray

    let map (f : 'a -> 'b) (h : History<'a>) =
        { events = List.map (Event.map f) h.events; finished = h.finished }

    let collect (f : 'a -> #seq<'b>) (h : History<'a>) =
        { events = List.collect (fun v -> v |> Event.collect f |> Seq.toList) h.events; finished = h.finished }


    let choose (f : 'a -> Option<'b>) (h : History<'a>) =
        { events = List.choose (Event.choose f) h.events; finished = h.finished }

    let union (l : History<'a>) (r : History<'a>) =
        let rec intersperse (l : list<Event<'a>>) (r : list<Event<'a>>) =
            match l, r with
                | [], r -> r
                | l, [] -> l
                | l'::ls, r'::rs ->
                    if l'.time < r'.time then l' :: intersperse ls r
                    else r' :: intersperse l rs


        { events = intersperse l.events r.events; finished = l.finished || r.finished }

    let intersperse (l : History<'a>) (r : History<'b>) =
        let rec intersperse (l : list<Event<'a>>) (r : list<Event<'b>>) : list<Event<Either<'a, 'b>>> =
            match l, r with
                | l'::ls, r'::rs ->
                    if l'.time < r'.time then { value = Left l'.value; time = l'.time } :: intersperse ls r
                    else { value = Right r'.value; time = r'.time } :: intersperse l rs
                | [], r -> r |> List.map (fun e -> { value = Right e.value; time = e.time })
                | l, [] -> l |> List.map (fun e -> { value = Left e.value; time = e.time })


        { events = intersperse l.events r.events; finished = l.finished && r.finished }


    let intersperse3 (h0 : History<'a>) (h1 : History<'b>) (h2 : History<'c>) =
        let h01 = intersperse h0 h1
        let h = intersperse h01 h2

        h |> map (fun e ->
            match e with
                | Left (Left a) -> Choice1Of3 a
                | Left (Right a) -> Choice2Of3 a
                | Right a -> Choice3Of3 a
        )



    let tryLast (h : History<'a>) =
        h.events |> List.tryLast


// =============================================================
// Core
// =============================================================

type IStreamReader<'a> =
    inherit IDisposable
    inherit IAdaptiveObject
    abstract member GetHistory : unit -> History<'a>
    abstract member SubscribeOnEvaluate : (History<'a> -> unit) -> IDisposable
    abstract member Finish : unit -> list<Event<'a>>

[<AllowNullLiteral>]
type astream<'a> =
    abstract member GetReader : unit -> IStreamReader<'a>

[<AutoOpen>]
module Utilities =
    let cancel() = raise <| OperationCanceledException()

// =============================================================
// Readers
// =============================================================
module AStreamReaders =

    [<AbstractClass>]
    type AbstractStreamReader<'a>() =
        inherit AdaptiveObject()

        let callbacks = HashSet<History<'a> -> unit>()
        let mutable finished = false

        member x.Callbacks = callbacks

        abstract member Release : unit -> unit
        abstract member ComputeHistory : unit -> History<'a>
        abstract member GetHistory : unit -> History<'a>
        abstract member Finish : unit -> list<Event<'a>>

        default x.Finish() = []

        default x.GetHistory() =
            x.EvaluateIfNeeded History.empty (fun () ->
                try
                    let h = x.ComputeHistory()

                    if not (History.isEmpty h) then
                        for cb in callbacks do cb h

                    if History.isFinished h then
                        x.Dispose()

                    h
                with :? OperationCanceledException ->
                    Log.warn "uncaught cancel"
                    { finished = true; events = []}
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
            member x.GetHistory() = 
                if finished then { x.GetHistory() with finished = true }
                else x.GetHistory()

            member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb
            member x.Finish() = x.Finish()

    let private wrap scope f =
        fun v -> Ag.useScope scope (fun () -> f v)
         
    type MapReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> list<'b>) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()

        override x.ComputeHistory() =
            let input = source.GetHistory()
            
            try
                input |> History.collect f
            with :? OperationCanceledException ->
                History.finished

    type ChooseReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> Option<'b>) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()

        override x.ComputeHistory() =
            let input = source.GetHistory() 
            
            try
                input |> History.choose f
            with :? OperationCanceledException ->
                History.finished

    type IntersperseReader<'a, 'b>(l : IStreamReader<'a>, r : IStreamReader<'b>) as this =
        inherit AbstractStreamReader<Either<'a, 'b>>()
        do l.AddOutput this; r.AddOutput this  


        override x.Release() =
            l.RemoveOutput this
            r.RemoveOutput this
            l.Dispose()
            r.Dispose()

        override x.ComputeHistory() =
            let lh = l.GetHistory()
            let rh = r.GetHistory()

            History.intersperse lh rh

    type IntersperseReader<'a, 'b, 'c>(r0 : IStreamReader<'a>, r1 : IStreamReader<'b>, r2 : IStreamReader<'c>) as this =
        inherit AbstractStreamReader<Choice<'a, 'b, 'c>>()
        do r0.AddOutput this; r1.AddOutput this; r2.AddOutput this


        override x.Release() =
            r0.RemoveOutput this
            r1.RemoveOutput this
            r2.RemoveOutput this
            r0.Dispose()
            r1.Dispose()
            r2.Dispose()

        override x.ComputeHistory() =
            let h0 = r0.GetHistory()
            let h1 = r1.GetHistory()
            let h2 = r2.GetHistory()

            History.intersperse3 h0 h1 h2

    type UnionReader<'a>(l : IStreamReader<'a>, r : IStreamReader<'a>) as this =
        inherit AbstractStreamReader<'a>()
        do l.AddOutput this; r.AddOutput this  


        override x.Release() =
            l.RemoveOutput this
            r.RemoveOutput this
            l.Dispose()
            r.Dispose()

        override x.ComputeHistory() =
            let lh = l.GetHistory()
            let rh = r.GetHistory()

            History.union lh rh

    type ConcatReader<'a>(l : IStreamReader<'a>, r : unit -> IStreamReader<'a>) as this =
        inherit AbstractStreamReader<'a>()
        do l.AddOutput this

        let mutable current = l

        override x.Finish() =
            if current = l then r().GetHistory().events
            else current.Finish()

        override x.Release() =
            current.RemoveOutput this
            current.Dispose()

        override x.ComputeHistory() =
            let h = current.GetHistory()

            if h.finished && current = l then
                current.RemoveOutput x
                try
                    current <- r()
                    current.AddOutput x

                    let rh = current.GetHistory()
                    History.union { finished = false; events = History.toList h } rh
                with :? OperationCanceledException ->
                    History.finished
            else
                h


    type CollectReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> IStreamReader<'b>) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        let dirtyInner = VolatileDirtySet(fun (r : IStreamReader<'b>) -> r, r.GetHistory())

        override x.InputChanged (o : IAdaptiveObject) = 
            match o with
                | :? IStreamReader<'b> as o -> dirtyInner.Add o
                | _ -> ()

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()
            let ips = x.Inputs |> Seq.toList
            for i in ips do 
                i.RemoveOutput x
                i.TryDispose() |> ignore

            dirtyInner.Clear()

        override x.ComputeHistory() =
            let xs = source.GetHistory()

            let events = SortedSet<Event<'b>>()
            let mutable fin = xs.finished
            try

                let histories = dirtyInner.Evaluate()

                for (r, history) in histories do
                    for e in History.toList history do
                        events.Add e |> ignore

                    if history.finished then
                        dirtyInner.Remove r
                        r.RemoveOutput x

                for e in History.toList xs do
                    let r = f e.value
                    for e in r.GetHistory() |> History.toList do 
                        events.Add e |> ignore
                    r.AddOutput x
                    dirtyInner.Add r

            with :? OperationCanceledException ->
                fin <- true

            { finished = fin; events = events |> Seq.toList }


    type BindReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> IStreamReader<'b>) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        let mutable current : Option<IStreamReader<'b>> = None

        override x.Finish() =
            match current with
                | Some c -> c.Finish()
                | None -> []

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()
            match current with
                | Some c -> c.RemoveOutput c
                | None -> ()

        override x.ComputeHistory() =
            let xs = source.GetHistory()

            match current with
                | Some c -> c.RemoveOutput x
                | None -> ()

            let mutable currentFinished = false
            let events = SortedSet<Event<'b>>()
            for last in History.toList xs do
                let newReader = f last.value
                match current with
                    | Some r -> 
                        let fin = r.Finish()
                        r.Dispose()
                        for f in fin do events.Add f |> ignore
                    | None -> ()

                current <- Some newReader
                let newHistory = newReader.GetHistory()
                for f in newHistory.events do events.Add f |> ignore
                if newHistory.finished then 
                    current <- None

            match current with
                | Some c -> 
                    c.AddOutput x
                    let newHistory = c.GetHistory()
                    for f in newHistory.events do events.Add f |> ignore
                    if newHistory.finished then 
                        current <- None
                | None -> ()

                
            { finished = xs.finished; events = Seq.toList events }



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

        override x.Finish() =
            inputReader.Finish()

        override x.GetHistory() =
            lock inputReader (fun () ->
                x.EvaluateIfNeeded History.empty (fun () ->
                    let h = x.ComputeHistory()

                    if not (History.isEmpty h) then
                        for cb in x.Callbacks do cb h

                    if History.isFinished h then
                        x.Dispose()

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

    type EmitReader<'a>(lockObj : obj, dispose : EmitReader<'a> -> unit) =
        inherit AbstractStreamReader<'a>()

        let buffer = List<Event<'a>>()
        let mutable finished = false

        member x.Emit (d : list<Event<'a>>) =
            lock x (fun () ->
                buffer.AddRange d

                if not x.OutOfDate then
                    match getCurrentTransaction() with
                        | Some t ->
                            t.Enqueue x
                        | _ ->
                            failwith "[EmitReader] cannot emit without transaction"
            )

        member x.Close() =
            lock x (fun () ->
                finished <- true
                if not x.OutOfDate then
                    match getCurrentTransaction() with
                        | Some t ->
                            t.Enqueue x
                        | _ ->
                            failwith "[EmitReader] cannot emit without transaction"
            )

        override x.Release() =
            dispose x
            buffer.Clear()

        override x.ComputeHistory() =
            let res = buffer |> Seq.toList
            buffer.Clear()
            { events = res; finished = finished }

    type OneShotReader<'a>(deltas : list<Event<'a>>) =  
        inherit AbstractStreamReader<'a>()
        
        let mutable deltas = deltas

        override x.ComputeHistory() =
            let res = deltas
            deltas <- []
            { events = res; finished = true }

        override x.Release() =
            deltas <- []



    type AllReader<'a>(source : IStreamReader<'a>) as this =
        inherit ASetReaders.AbstractReader<'a>()
        do source.AddOutput this

        override x.ComputeDelta() =
            source.GetHistory() |> History.toList |> List.map (Event.value >> Add)

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()
     
    type DeltaReader<'a>(source : IStreamReader<Delta<'a>>) as this =
        inherit ASetReaders.AbstractReader<'a>()
        do source.AddOutput this

        override x.ComputeDelta() =
            source.GetHistory() |> History.toList |> List.map (Event.value)

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()


    type PartitionReader<'a>(condition : IStreamReader<bool>, inner : IStreamReader<'a>) as this =
        inherit AbstractStreamReader<IStreamReader<'a>>()
        do condition.AddOutput this; inner.AddOutput this

        let mutable current : Option<EmitReader<'a>> = None //new EmitReader<'a>(this, ignore)
        let lockObj = obj()

        override x.ComputeHistory() =
            let h = History.intersperse (condition.GetHistory()) (inner.GetHistory())

            let readers = List<Event<IStreamReader<'a>>>()
            for e in History.toList h do
                match e.value with
                    | Left a ->
                        match current with
                            | Some c when not a ->
                                // got inactive
                                c.Close()
                                current <- None
                            | None when a ->
                                // got active 
                                let er = new EmitReader<'a>(lockObj, ignore)
                                
                                current <- Some er
                                readers.Add { time = e.time; value = er }

                            | _ -> 
                                ()
 
                        
                    | Right v ->
                        match current with
                            | Some c -> c.Emit [{ time = e.time; value = v }]
                            | _ -> ()

            match current with
                | Some c -> x.AddOutput c
                | None -> ()

            { finished = h.finished; events = Seq.toList readers }

        override x.Release() =
            condition.RemoveOutput this
            inner.RemoveOutput this
            current <- None
            condition.Dispose()
            inner.Dispose()

    type BinaryReader<'a, 'b, 'c>(op : 'a -> 'b -> 'c, da : 'a, db : 'b, l : IStreamReader<'a>, r : IStreamReader<'b>) as this =
        inherit AbstractStreamReader<'c>()
        do l.AddOutput this; r.AddOutput this

        let mutable currentLeft = da
        let mutable currentRight = db

        override x.ComputeHistory() =
            let lh = l.GetHistory()
            let rh = r.GetHistory()

            let h = History.intersperse lh rh

            let events = 
                h |> History.toList
                  |> List.map (fun e ->
                    match e.value with
                        | Left v ->currentLeft <- v
                        | Right v -> currentRight <- v

                    { time = e.time; value = op currentLeft currentRight }
                  )

            { finished = lh.finished && rh.finished; events = events }




        override x.Release() =
            l.RemoveOutput x
            r.RemoveOutput x
            l.Dispose()
            r.Dispose()




// =============================================================
// AStream
// =============================================================

module AStream =
    open AStreamReaders

    type ConstantStream<'a>(content : list<Event<'a>>) =

        interface astream<'a> with
            member x.GetReader () =
                let r = new OneShotReader<'a>(content)
                r :> IStreamReader<_>


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

                


    let single (v : 'a) =
        ConstantStream [{ time = History.now(); value = v }] :> astream<_>

    let ofSeq (v : seq<'a>) =
        ConstantStream (v |> Seq.map (fun v -> { time = History.now(); value = v }) |> Seq.toList) :> astream<_>

    let ofList (v : list<'a>) =
        ConstantStream (v |> List.map (fun v -> { time = History.now(); value = v })) :> astream<_>

    let ofArray (v : 'a[]) =
        ofSeq v


    let toASet (stream : astream<'a>) : aset<'a> =
        ASet.AdaptiveSet(fun () -> new AllReader<_>(stream.GetReader()) :> _) :> _

    let latest (stream : astream<'a>) : IMod<Option<'a>> =
        let r = stream.GetReader()
        let mutable current = None
        [r :> IAdaptiveObject] |> Mod.mapCustom (fun () ->
            match r.GetHistory() |> History.tryLast with
                | Some { value = l } -> 
                    current <- Some l
                    Some l
                | None ->
                    current
        )



    let map (f : 'a -> 'b) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new MapReader<_,_>(scope, stream.GetReader(), fun v -> [f v]) :> _) :> _

    let collectSeq (f : 'a -> #seq<'b>) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new MapReader<_,_>(scope, stream.GetReader(), fun v -> Seq.toList (f v)) :> _) :> _

    let collect (f : 'a -> astream<'b>) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new CollectReader<_,_>(scope, stream.GetReader(), fun v -> (f v).GetReader()) :> _) :> _

    let bind (f : 'a -> astream<'b>) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new BindReader<_,_>(scope, stream.GetReader(), fun v -> (f v).GetReader()) :> _) :> _


    let intersperse (l : astream<'a>) (r : astream<'b>) =
        AdaptiveStream(fun () -> new IntersperseReader<_,_>(l.GetReader(),r.GetReader()) :> _) :> astream<_>

    let intersperse3 (s0 : astream<'a>) (s1 : astream<'b>) (s2 : astream<'c>) =
        AdaptiveStream(fun () -> new IntersperseReader<_,_,_>(s0.GetReader(),s1.GetReader(),s2.GetReader()) :> _) :> astream<_>

    let union (l : astream<'a>) (r : astream<'a>) =
        AdaptiveStream(fun () -> new UnionReader<_>(l.GetReader(),r.GetReader()) :> _) :> astream<_>

    let concat (l : astream<'a>) (r : unit -> astream<'a>) =
        AdaptiveStream(fun () -> new ConcatReader<_>(l.GetReader(),fun () -> r().GetReader()) :> _) :> astream<_>


    let choose (f : 'a -> Option<'b>) (stream : astream<'a>) : astream<'b>=
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new ChooseReader<_,_>(scope, stream.GetReader(), f) :> _) :> _

    let fold (f : 's -> 'a -> 's) (seed : 's) (stream : astream<'a>) : IMod<'s> =
        let r = stream.GetReader()
        let mutable result = seed
        [r :> IAdaptiveObject] 
            |> Mod.mapCustom (fun () ->
                let h = r.GetHistory() |> History.toList |> List.map Event.value

                result <- List.fold f result h
                result
            )

    let splitWhile (cond : astream<bool>) (inner : astream<'a>) : astream<astream<'a>> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> 
            new MapReader<_,_>(scope, new PartitionReader<_>(cond.GetReader(), inner.GetReader()), fun s -> 
                [AdaptiveStream(fun () -> new CopyReader<_>(s, ignore) :> IStreamReader<_>) :> astream<_>]
            ) :> IStreamReader<_>
        ) :> astream<_>

    let logicAnd (l : astream<bool>) (r : astream<bool>) =
        AdaptiveStream(fun () -> new BinaryReader<_,_,_>((&&), false, false, l.GetReader(), r.GetReader()) :> _ ) :> astream<_>

    let logicOr (l : astream<bool>) (r : astream<bool>) =
        AdaptiveStream(fun () -> new BinaryReader<_,_,_>((||), false, false, l.GetReader(), r.GetReader()) :> _ ) :> astream<_>

    let logicXor (l : astream<bool>) (r : astream<bool>) =
        let inline xor a b = (a && not b) || (b && not a)
        AdaptiveStream(fun () -> new BinaryReader<_,_,_>(xor, false, false, l.GetReader(), r.GetReader()) :> _ ) :> astream<_>


[<AutoOpen>]
module ``AStream Builder`` =
    open System.Reflection

    let private getCopyReadersForClosure (f : 'a) : list<FieldInfo * IAdaptiveObject> =
        failwith ""

    let private limitStream(guard : astream<bool>) (stream : IAdaptiveObject) : IAdaptiveObject =
        failwith ""

    type StreamBuilder() =
        member x.For(s : astream<'a>, f : 'a -> astream<'b>) : astream<'b> =
            s |> AStream.collect f

        member x.Yield v = AStream.single v

        member x.Combine(l : astream<'a>, r : unit -> astream<'a>) =
            AStream.concat l r

        member x.Delay(f : unit -> 'a) = f

        member x.Run(f : unit -> 'a) = f()

        member x.Zero() = AStream.ofList []

        member x.Return v = AStream.single v



            


    let astream = StreamBuilder()

    module ASet =
        let ofStream (s : astream<'a>) =
            AStream.toASet s

        let ofDeltaStream (s : astream<Delta<'a>>) =
            ASet.AdaptiveSet(fun () -> new AStreamReaders.DeltaReader<_>(s.GetReader()) :> IReader<_>) :> aset<_>


// =============================================================
// CStream a.k.a Event
// =============================================================

type cstream<'a>() =
    let readers = WeakSet<AStreamReaders.EmitReader<'a>>()
    

    member x.GetReader() =
        lock readers (fun () ->
            let r = new AStreamReaders.EmitReader<'a>(x, readers.Remove >> ignore)
            readers.Add r |> ignore

            r :> IStreamReader<_>
        )

    member x.Push(v : 'a) =
        let events = [{ value = v; time = History.now() }]
        for r in readers do
            r.Emit events

    member x.Push(values : seq<'a>) =
        let events = values |> Seq.map (fun v -> { value = v; time = History.now() }) |> Seq.toList
        for r in readers do
            r.Emit events

    member x.Push(values : list<'a>) =
        let events = values |> List.map (fun v -> { value = v; time = History.now() })
        for r in readers do
            r.Emit events


    interface astream<'a> with
        member x.GetReader() = x.GetReader()

module CStream =
    let empty<'a> = cstream<'a>()

    let inline push (v : 'a) (s : cstream<'a>) = s.Push v

    let inline pushMany (v : seq<'a>) (s : cstream<'a>) = s.Push v

[<AutoOpen>]
module CStreamOperators =
    
    let inline (<==) (s : cstream<'a>) (value : 'a) =
        s.Push value

    let inline (==>) (value : 'a) (s : cstream<'a>) =
        s.Push value

    let inline (<<=) (s : cstream<'a>) (values : seq<'a>) =
        s.Push values

    let inline (=>>) (values : seq<'a>) (s : cstream<'a>) =
        s.Push values

    let inline (>|<) (l : astream<'a>) (r : astream<'a>) =
        AStream.union l r

    let inline (>.<) (l : astream<'a>) (r : astream<'b>) =
        AStream.intersperse l r

    let inline (<?-) (r : astream<'a>) (l : astream<bool>) =
        AStream.splitWhile l r

    let inline (-?>) (l : astream<bool>) (r : astream<'a>) =
        AStream.splitWhile l r

    let inline (.&.) (l : astream<bool>) (r : astream<bool>) =
        AStream.logicAnd l r

    let inline (.|.) (l : astream<bool>) (r : astream<bool>) =
        AStream.logicOr l r

    let inline (.^.) (l : astream<bool>) (r : astream<bool>) =
        AStream.logicXor l r

type Result<'a, 'b> = 
    | Intermediate of 'a
    | Final of 'b

type private WorkflowReader<'a, 'b>(inner : IStreamReader<Result<'a, 'b>>) as this =
    inherit AStreamReaders.AbstractStreamReader<'b>()
    do inner.AddOutput this

    let mutable fin = false
    let buffer = System.Collections.Generic.List<Event<'b>>()
    let mutable last = None

    let compute() =
        let h = inner.GetHistory()

        for e in History.toList h do
            match e.value with
                | Intermediate v ->
                    last <- Some v
                | Final v ->
                    buffer.Add { time = e.time; value = v }

        fin <- false


        last

    let intermediate =
        [this :> IAdaptiveObject]
            |> Mod.mapCustom (fun () ->
                compute()
            )

    member x.Intermediate = intermediate

    override x.ComputeHistory() =
        compute() |> ignore

        let events2 = buffer |> Seq.toList
        buffer.Clear()
            
        { events = events2; finished = fin }


    override x.Release() =
        inner.RemoveOutput this
        inner.Dispose()

type Workflow<'i, 'f> internal(stream : astream<Result<'i, 'f>>) =

    let r = lazy ( new WorkflowReader<_,_>(stream.GetReader()) )
    let intermediate = Mod.dynamic (fun () -> r.Value.Intermediate)
    let final = AStream.AdaptiveStream(fun () -> r.Value :> IStreamReader<_>) :> astream<_>

    member x.Stream = stream
    member x.Intermediate = intermediate
    member x.Final = final

type Workflow<'a> = Workflow<'a, 'a>

module Workflow =
    open AStreamReaders


    let ofAStream (stream : astream<Result<'i, 'f>>) : Workflow<'i, 'f> =
        new Workflow<_,_>(stream)

    let inline intermediate (wf : Workflow<'i, 'f>) = wf.Intermediate
    let inline final (wf : Workflow<'i, 'f>) = wf.Final

[<AutoOpen>]
module ``Workflow Builder`` =
    [<AutoOpen>]
    module private BlackMagic =
        open System.Reflection
        open Microsoft.FSharp.Quotations
        open FSharp.Quotations.Evaluator

        let getClosureRefs (f : 'a) =
            let fields = f.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
            fields |> Array.filter (fun fi -> fi.FieldType.IsGenericType && fi.FieldType.GetGenericTypeDefinition() = typedefof<ref<_>>)
                   |> Array.map (fun rf -> rf.GetValue(f))

        let compileClosureReset (exclude : obj[]) (f : 'a)=
            let fields = f.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
            let excluded = System.Collections.Generic.HashSet(exclude)

            let resetRefs = 
                fields |> Array.filter (fun fi -> fi.FieldType.IsGenericType && fi.FieldType.GetGenericTypeDefinition() = typedefof<ref<_>>)
                       |> Array.choose (fun rf ->
                            let r = rf.GetValue(f) 
                            if excluded.Contains r then 
                                None
                            else
                                let contents = r.GetType().GetProperty("Value")
                                let v = contents.GetValue(r)
                                let reset = 
                                    Expr.Lambda(Var("unitVar", typeof<unit>), 
                                        Expr.PropertySet(
                                            Expr.Value(r, rf.FieldType),
                                            contents,
                                            Expr.Value(v, contents.PropertyType)
                                        )
                                    )

                                reset.CompileUntyped() |> unbox<unit -> unit> |> Some
                        )
                   
            let reset() =
                for r in resetRefs do
                    r()

            reset

    type Reader<'s, 'a> = { runReader : 's -> 'a}
    type State = { filter : astream<bool>; globalRefs : obj[] }

    type WorkflowBuilder() =
        member x.For(s : astream<'a>, f : 'a -> seq<'b>) : Reader<State, (unit -> seq<'b>) -> astream<'b>> =
            { runReader =
                fun state ->
                    let reset = compileClosureReset state.globalRefs f
                    fun final ->  
                        let mutable isActive = false
                        AStream.intersperse state.filter s 
                            |> AStream.collectSeq (fun v ->
                                match v with
                                    | Left f ->
                                        let res = 
                                            if isActive && not f then
                                                final()
                                            elif not isActive && f then
                                                reset()
                                                Seq.empty
                                            else
                                                Seq.empty

                                

                                        isActive <- f
                                        res
                                    | Right v ->
                                        if isActive then
                                    
                                            f v
                                        else
                                            Seq.empty
                            )
            }

        member x.Yield v = Seq.singleton (Intermediate v)

        member x.Combine(l : seq<'a>, r : unit -> seq<'a>) =
            seq {
                for e in l do yield e
                for e in r() do yield e
            }

        member x.Combine(l : Reader<State, (unit -> seq<'a>) -> astream<'a>>, r : unit -> seq<'a>) : Reader<State, astream<'a>> =
            { runReader =
                fun state -> l.runReader state r
            }


        member x.Delay(f : unit -> 'a) = f

        member x.Run(f : unit -> astream<Result<'a, 'b>>) = f() |> Workflow.ofAStream

        member x.Zero() = Seq.empty

        member x.Return v = Seq.singleton (Final v)

        member x.While(guard : unit -> astream<bool>, body : unit -> Reader<State, astream<'a>>) : astream<'a> =
            let globalRefs = getClosureRefs body
            (body ()).runReader { filter = guard(); globalRefs = globalRefs }

    let workflow = WorkflowBuilder()




module Sketch2 =

    type WorkflowBuilder() =
        let create s = Workflow<_,_>(s)


        member x.Bind(wf : Workflow<_, 'a>, f : 'a -> Workflow<'i, 'f>) =
            wf.Stream |> AStream.collect (fun v ->
                match v with
                    | Intermediate _ -> AStream.ofList []
                    | Final v ->
                        let inner = (f v).Stream
                        inner
            ) |> create

//        member x.For(wf : Workflow<_,'a>, f : 'a -> Workflow<'i, 'f>) : Workflow<'i, 'f> =
//            wf.Stream |> AStream.collect (fun v ->
//                match v with
//                    | Intermediate _ -> AStream.ofList []
//                    | Final v ->
//                        let inner = (f v).Stream
//                        inner
//            ) |> create

        member x.For(wf : Workflow<'ia,'a>, f : Result<'ia, 'a> -> Workflow<'i, 'f>) : Workflow<'i, 'f> =
            wf.Stream |> AStream.collect (fun v ->
                (f v).Stream
            ) |> create

        member x.Yield(v : 'a) = [Intermediate v] |> AStream.ofList |> create
        member x.Return(v : 'a) = [Final v] |> AStream.ofList |> create

        member x.Delay(f : unit -> Workflow<'a, 'b>) =
            f

        member x.Zero() = [] |> AStream.ofList |> create

        member x.Combine(l : Workflow<'i, 'f>, r : unit -> Workflow<'i, 'f>) =
            AStream.concat l.Stream (fun () -> r().Stream) |> create

        member x.Run(f : unit -> Workflow<'i, 'f>) = f()


    let wf = WorkflowBuilder()

    let test (active : astream<bool>) (input : astream<V2d>) =
        astream {
            let mutable isActive = false
            let mutable current = []

            for i in AStream.intersperse active input do
                match i with
                    | Left v ->
                        if isActive && not v then
                            yield Final (current |> List.rev |> List.toArray)
                            yield Intermediate []
                            current <- []
                        isActive <- v

                    | Right value ->
                        if isActive then
                            current <- value::current
                            yield Intermediate current
            }

    let takeWhile (active : astream<bool>) (input : astream<'a>) =
        astream {
            let mutable isActive = false
            let current = List<'a>()

            for i in AStream.intersperse active input do
                match i with
                    | Left v ->
                        if isActive && not v then
                            yield Final (Seq.toList current)
                            yield Intermediate []
                            current.Clear()
                        isActive <- v

                    | Right value ->
                        if isActive then
                            current.Add value
                            yield Intermediate (Seq.toList current)
            } |> Workflow.ofAStream


    let sketching (viewProjTrafo : IMod<Trafo3d>) (input : Workflow<list<PixelPosition>>) =
        wf {
            for res in input do
                match res with
                    | Final poly -> 
                        let vp = Mod.force viewProjTrafo

                        let worldPoly = 
                            poly |> List.map (fun pp -> 
                                let p = pp.NormalizedPosition
                                vp.Backward.TransformPosProj(V3d(2.0 * p.X - 1.0, 1.0 - 2.0 * p.Y, 0.0))
                            )

                        System.Threading.Thread.Sleep(1000)
                        return Polygon3d(worldPoly)
                    | Intermediate v ->
                        yield v
        }
        

    let runSketching () =
        let active = CStream.empty
        let clicks = CStream.empty
        let trafo = Mod.init Trafo3d.Identity

        let sketchedPolys = 
            clicks |> takeWhile active 
                   |> sketching trafo

        let currentSketchPoly =
            adaptive {
                let! p = sketchedPolys.Intermediate
                match p with
                    | Some p -> 
                        return p.PairChainWrap()  
                            |> Seq.collect (fun p -> [p.E0; p.E1]) 
                            |> Seq.map (fun pp -> pp.NormalizedPosition)
                            |> Seq.map (fun p -> V3f(float32 (2.0 * p.X - 1.0), float32(1.0 - 2.0 * p.Y), 0.0f))
                            |> Seq.toArray
                    | None -> 
                        return [||]
            }

        let sg = 
            aset {
                for f in sketchedPolys.Final |> AStream.toASet do
                
                    //build geometry and stuff
                    let indices = f.ComputeTriangulationOfConcavePolygon(1.0)
                    let vertices = f.Points |> Seq.map V3f |> Seq.toArray
                    yield (indices, vertices)
            }

        ()

    let run() =
        let active = CStream.empty
        let positions = CStream.empty

        let result = test active positions |> Workflow.ofAStream

        let final = result.Final |> AStream.toASet
        let r = final.GetReader()
        let current = result.Intermediate |> Mod.map (function Some m -> m | None -> [])


        current |> Mod.force  |> printfn "current: %A"
        r.Update(); r.Content |> Seq.toList |> printfn "final: %A"

        transact (fun () ->
            CStream.pushMany [true] active
        )

        current |> Mod.force  |> printfn "current: %A"
        r.Update(); r.Content |> Seq.toList |> printfn "final: %A"

        transact (fun () ->
            CStream.push V2d.II positions
            CStream.push V2d.OI positions
        )
        current |> Mod.force  |> printfn "current: %A"
        r.Update(); r.Content |> Seq.toList |> printfn "final: %A"


        transact (fun () ->
            CStream.push false active
        )
        current |> Mod.force  |> printfn "current: %A"
        r.Update(); r.Content |> Seq.toList |> printfn "final: %A"

        transact (fun () ->

            true ==> active
            positions <== V2d.IO
            active |> CStream.push false

            positions <<= [V2d.II; V2d.II]

            CStream.push true active
            CStream.push V2d.OI positions
            CStream.push false active
            CStream.push V2d.II positions
            CStream.push V2d.II positions
            CStream.push true active
            CStream.push V2d.IO positions
        )

        current |> Mod.force  |> printfn "current: %A"
        r.Update(); r.Content |> Seq.toList |> printfn "final: %A"




        ()

    open System.Reflection
    open Microsoft.FSharp.Quotations
    open FSharp.Quotations.Evaluator

    let resetClosure() =
        let mutable a = 10
        let f() =
            a <- a + 1

        let fields = f.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
        
        let resetRefs = 
            fields |> Array.filter (fun fi -> fi.FieldType.IsGenericType && fi.FieldType.GetGenericTypeDefinition() = typedefof<ref<_>>)
                   |> Array.map (fun rf ->
                        let r = rf.GetValue(f) 
                        let contents = r.GetType().GetProperty("Value")
                        let v = contents.GetValue(r)

                        let reset = 
                            Expr.Lambda(Var("unitVar", typeof<unit>), 
                                Expr.PropertySet(
                                    Expr.Value(r, rf.FieldType),
                                    contents,
                                    Expr.Value(v, contents.PropertyType)
                                )
                            )

                        reset.CompileUntyped() |> unbox<unit -> unit>
                    )
                   
        let reset() =
            for r in resetRefs do
                r()

        f()
        printfn "%A" a

        reset()
        f()
        printfn "%A" a

        ()


module Sketch666 =
    

    type WorkflowBuilder() =
        member x.For(s : astream<'a>, f : 'a -> astream<'b>) : astream<'b> =
            s |> AStream.collect f

        member x.Yield v = AStream.single (Intermediate v)

        member x.Return v = AStream.single (Final v)

        member x.Combine(l : astream<'a>, r : unit -> astream<'a>) =
            AStream.concat l r

        member x.Delay(f : unit -> 'a) = f

        member x.Run(f : unit -> astream<Result<'a, 'b>>) = f() |> Workflow.ofAStream

        member x.Zero() = AStream.ofList []

    let workflow = WorkflowBuilder()

    let run() =
        let active = CStream.empty
        let values = CStream.empty


        let rebuilt =
            workflow {
                // partition the stream into its parts
                // while active "is" true
                // in detail this means that the latest value of active is true
                for s in values <?- active do
                    // for every substream we create a list accumulating 
                    // its values
                    let result = List()


                    // for every new value in our substream we'd like to
                    // add it to the current list (e.g. a polygon)
                    for v in s do
                        result.Add v

                        // furthermore we could want to visualize the current
                        // state somehow which means that we need to
                        // "yield" an intermediate result replacing the
                        // last one (if any)
                        yield Seq.toList result

                    // when we're done with our substream (active became false)
                    // we'd like to return the final result.
                    // Note that the intermediate- and final types do not 
                    // necessarily need to match. 
                    return Seq.toList result
            }


        // the workflow can give us the latest intermediate-value
        let intermediate : IMod<Option<list<int>>> = rebuilt.Intermediate

        // and also a stream of all final values
        let final : astream<list<int>> = rebuilt.Final


        // but for the moment we'll stick with the stream of all
        // results containing a union-type (Result<'a, 'b>) encoding 
        // for the kind of every result.
        let r = rebuilt.Stream.GetReader()


        // handy operators allow us to emit values in a
        // very concise way. 
        transact (fun () ->
            active <== true
            values <<= [1; 2]
            active <== false
            values <== 2
            active <== true
            values <== 3
            active <== false
            active <== true
            values <== 4
        )


        r.GetHistory() |> History.toList |> printfn "%A"

        transact (fun () ->
            values <== 5
        )

        r.GetHistory() |> History.toList |> printfn "%A"

        transact (fun () ->
            active <== false
            values <== 6
            active <== true
            values <<= [1;2]
            values <== 3
            active <== false
        )
        r.GetHistory() |> History.toList |> printfn "%A"


        ()





//module Sketch3 =
//    
//    type CreatorState = Option<astream<bool>>
//
//    type StreamCreator<'a> = { create : CreatorState -> (unit -> astream<'a>) -> astream<'a> }
//
//
//    type StreamBuilder() =
//        member x.For(s : astream<'a>, f : 'a -> StreamCreator<'b>) : StreamCreator<'b> =
//            { create = fun state cont ->
//                s |> AStream.collect (fun v ->
//                    (f v).create state cont
//                )
//            }
//
//        member x.Combine(l : StreamCreator<'a>, r : StreamCreator<'a>) =
//            { create = fun state cont ->
//                l.create state (fun () ->
//                    r.create state cont
//                )
//            }
//
//
//
//        member x.While()
