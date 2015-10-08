
#I @"..\..\..\bin\Release"
#r @"Aardvark.Base.dll"
#r @"Aardvark.Base.Essentials.dll"
#r @"Aardvark.Base.TypeProviders.dll"
#r @"Aardvark.Base.FSharp.dll"
#r @"Aardvark.Base.Incremental.dll"

open System
open Aardvark.Base
open Aardvark.Base.Incremental

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
        static let instance : History<'a> = { events = []; finished = false }
        static member Instance = instance

    let empty<'a> : History<'a> = EmptyHistoryImpl<'a>.Instance

    let inline isFinished (h : History<'a>) = h.finished

    let inline isEmpty (h : History<'a>) = h.events |> List.isEmpty

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


type IStreamReader<'a> =
    inherit IDisposable
    inherit IAdaptiveObject
    abstract member GetHistory : unit -> History<'a>
    abstract member SubscribeOnEvaluate : (History<'a> -> unit) -> IDisposable

[<AllowNullLiteral>]
type astream<'a> =
    abstract member GetReader : unit -> IStreamReader<'a>


module AStreamReaders =
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

                if History.isFinished h then
                    x.Dispose()

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
         
    type MapReader<'a, 'b>(scope, source : IStreamReader<'a>, f : 'a -> list<'b>) as this =
        inherit AbstractStreamReader<'b>()
        do source.AddOutput this  
        let f = wrap scope f

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()

        override x.ComputeHistory() =
            source.GetHistory() |> History.collect f

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
            source.GetHistory() |> History.toList |> List.map (Event.value >> Add)

        override x.Release() =
            source.RemoveOutput this
            source.Dispose()
          
    type PartitionReader<'a>(active : IStreamReader<bool>, org : IStreamReader<'a>) as this =
        inherit AbstractStreamReader<History<'a>>()

        do active.AddOutput this
           org.AddOutput this


        let buffer = List<Event<History<'a>>>()
        let mutable isActive = false
        let mutable latest = History.empty


        let compute() =
            this.EvaluateIfNeeded latest (fun () ->
                let activeHistory = active.GetHistory()
                let valueHistory = org.GetHistory()

                let h = History.intersperse activeHistory valueHistory

            
                let current = List()
                let values =
                    [
                        for e in History.toList h do
                            match e.value with
                                | Left a -> 
                                    if not a && isActive && current.Count > 0 then
                                        yield { value = { events = Seq.toList current; finished = true }; time = e.time }
                                        current.Clear()

                                    isActive <- a

                                | Right v ->
                                    if isActive then
                                        current.Add { value = v; time = e.time }
                    ]


                let last = { events = Seq.toList current; finished = false }

                buffer.AddRange values
                latest <- last
                last
            )

        let activeMod = [this :> IAdaptiveObject] |> Mod.mapCustom (fun () -> compute())

        member x.Active = activeMod


        override x.ComputeHistory() =
            compute() |> ignore

            let h = buffer |> Seq.toList
            buffer.Clear()

            { events = h; finished = false }

        override x.Release() =
            ()


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




module AStream =
    open AStreamReaders

    let map (f : 'a -> 'b) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new MapReader<_,_>(scope, stream.GetReader(), fun v -> [f v]) :> _) :> _

    let collect (f : 'a -> #seq<'b>) (stream : astream<'a>) : astream<'b> =
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new MapReader<_,_>(scope, stream.GetReader(), fun v -> Seq.toList (f v)) :> _) :> _

    let intersperse (l : astream<'a>) (r : astream<'b>) =
        AdaptiveStream(fun () -> new IntersperseReader<_,_>(l.GetReader(),r.GetReader()) :> _) :> astream<_>

    let intersperse3 (s0 : astream<'a>) (s1 : astream<'b>) (s2 : astream<'c>) =
        AdaptiveStream(fun () -> new IntersperseReader<_,_,_>(s0.GetReader(),s1.GetReader(),s2.GetReader()) :> _) :> astream<_>


    let choose (f : 'a -> Option<'b>) (stream : astream<'a>) : astream<'b>=
        let scope = Ag.getContext()
        AdaptiveStream(fun () -> new ChooseReader<_,_>(scope, stream.GetReader(), f) :> _) :> _

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

    let all (stream : astream<'a>) : aset<'a> =
        ASet.AdaptiveSet(fun () -> new AllReader<_>(stream.GetReader()) :> _) :> _

    let partition (m : astream<bool>) (stream : astream<'a>) : astream<History<'a>> * IMod<History<'a>> =
        let r = lazy ( new PartitionReader<_>(m.GetReader(), stream.GetReader()) )

        let final = new AdaptiveStream<_>(fun () -> r.Value :> IStreamReader<_>) :> astream<_>
        let current = Mod.dynamic (fun () -> r.Value.Active)

        final, current

    let fold (f : 's -> 'a -> 's) (seed : 's) (stream : astream<'a>) : IMod<'s> =
        let r = stream.GetReader()
        let mutable result = seed
        [r :> IAdaptiveObject] 
            |> Mod.mapCustom (fun () ->
                let h = r.GetHistory() |> History.toList |> List.map Event.value

                result <- List.fold f result h
                result
            )


type cstream<'a>() =
    let readers = WeakSet<AStreamReaders.EmitReader<'a>>()
    

    member x.GetReader() =
        lock readers (fun () ->
            let r = new AStreamReaders.EmitReader<'a>(x, readers.Remove >> ignore)
            readers.Add r |> ignore

            r :> IStreamReader<_>
        )

    member x.Push(v : 'a) =
        let now = History.now()
        for r in readers do
            r.Emit [{ value = v; time = now }]

    interface astream<'a> with
        member x.GetReader() = x.GetReader()

module CStream =
    let empty<'a> = cstream<'a>()

    let inline push (v : 'a) (s : cstream<'a>) = s.Push v


//    let final (f : 'a -> 'r) (stream : astream<'a>) : IMod<Option<'r>> =
//        failwith ""
//
//    let fold2 (final : 's -> 'r) (f : 's -> 'a -> 's) (seed : 's) (stream : astream<'a>) : Either<IMod<'s>, 'r> =
//        failwith ""
//
//    let whenFinished (f : list<Event<'a>> -> list<'b>) (stream : astream<astream<'a>>) : astream<'b> =
//        failwith ""

module Sketch =
    

    type Workflow<'i, 'f> = { intermediate : IMod<Option<'i>>; final : astream<'f> }

    type WorkflowCreator<'i, 'f> = { create : list<IMod<bool>> -> Workflow<'i, 'f> }

    type WorkflowB<'i, 'f> = 
        | Intermediate of (list<IMod<bool>> -> IMod<Option<'i>>)
        | Final of (unit -> 'f)


    type Reader<'s, 'a> = { runReader : 's -> 'a }

    type WorkflowBuilder() =
        

        member x.For(s : astream<'a>, f : 'a -> seq<'b>) =
            { runReader = fun filter ->
                let s, active = s |> AStream.partition filter
                let intermediate = 
                    active
                      |> Mod.map (fun o ->
                            match History.tryLast o with
                                | Some last -> last.value |> f |> Seq.last |> Some
                                | None -> None
                      )    
                intermediate, s
            }

        member x.Return (v : 'a) =
            [v]

        member x.Delay (f : unit -> 'a) = f

        member x.Yield(v) = Seq.singleton v

        member x.Combine(l : Reader<'s, IMod<Option<'a>> * astream<History<_>>>, r : unit -> list<'b>) =
            { runReader = fun filter ->
                let l = l.runReader filter
                { intermediate = fst l; final = l |> snd |> AStream.map (fun _ -> r()) }
            }

        member x.Combine(l : list<'a>, r : unit -> list<'a>) = l @ r()

        member x.While(guard : unit -> astream<bool>, body : unit -> Reader<astream<bool>,Workflow<'i, 'f>>) =
            body().runReader (guard())

        member x.Run(f : unit -> 'a) = f()

//        member x.For(s : astream<astream<'a>>, f : 'a -> seq<'b>) =
//            s |> AStream.map (AStream.latest >> Mod.map (Option.map (f >> Seq.last)))
//              |> AStream.latest
//              |> Mod.bind (fun o ->
//                match o with
//                    | Some o -> o
//                    | None -> Mod.constant None
//              )
//
//        member x.Return (v : 'a) =
//            
//        member x.Yield (v : 'a) = Seq.singleton v

    let wf = WorkflowBuilder()

    let test (m : astream<bool>) (s : astream<V2i>) =
        wf {
            while m do
                let mutable poly = []
                for e in s do
                    poly <- e::poly
                    yield poly

                return Polygon2d(poly |> List.map V2d)
        }

module Sketch2 =
    
    type Workflow<'i, 'f> = { intermediate : IMod<Option<'i>>; final : astream<'f> }

    type Result<'a, 'b> = 
        | Intermediate of 'a
        | Final of 'b

    type RefContext() =
        let refs = System.Collections.Generic.List<obj * obj>()
      
        let newRef (value : 'a) =
            let r = ref value
            refs.Add(r :> obj, value :> obj)
            r

        let reset () =
            for (r,v) in refs do
                let p = r.GetType().GetProperty("Value")
                p.SetValue(r, v)

        member x.New(value : 'a) =
            newRef value

        member x.Reset() =
            reset()

    type State = astream<bool> * RefContext

    type StreamBuilder() =
        member x.For(s : astream<'a>, f : 'a -> seq<'b>) : astream<'b> =
            s |> AStream.collect f

        member x.For((a,b), f : Either<'a, 'b> -> seq<'c>) : astream<'c> =
            AStream.intersperse a b |> AStream.collect f

        member x.For((a,b,c), f : Choice<'a, 'b, 'c> -> seq<'c>) : astream<'c> =
            AStream.intersperse3 a b c |> AStream.collect f


        member x.Yield v = Seq.singleton (Intermediate v)

        member x.Combine(l : seq<'a>, r : unit -> seq<'a>) =
            seq {
                for e in l do yield e
                for e in r() do yield e
            }

        member x.Delay(f : unit -> 'a) = f

        member x.Run(f : unit -> 'a) = f()

        member x.Zero() = Seq.empty

        member x.Return v = Seq.singleton (Final v)


        member x.While(guard : unit -> astream<bool>, body : unit -> State -> astream<Result<'a, 'b>>) : astream<Result<'a, 'b>> =
            failwith ""

    type NewRef<'a> = { initial : 'a }

    type MagicStreamBuilder() =
        member x.For(s : astream<'a>, f : 'a -> seq<'b>) : State -> (unit -> seq<'b>) -> astream<'b> =
            fun (filter,ctx) final -> 
                let mutable isActive = false
                AStream.intersperse filter s 
                    |> AStream.collect (fun v ->
                        match v with
                            | Left f ->
                                let res = 
                                    if isActive && not f then
                                        final()
                                    elif not isActive && f then
                                        ctx.Reset()
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



        member x.Yield v = Seq.singleton (Intermediate v)

        member x.Combine(l : seq<'a>, r : unit -> seq<'a>) =
            seq {
                for e in l do yield e
                for e in r() do yield e
            }

        member x.Combine(l : State -> (unit -> seq<'a>) -> astream<'a>, r : unit -> seq<'a>) : State -> astream<'a>=
            fun state ->
                l state r


        member x.Delay(f : unit -> 'a) = f

        member x.Run(f : unit -> 'a) = f()

        member x.Zero() = Seq.empty

        member x.Return v = Seq.singleton (Final v)

        member x.Bind(nr : NewRef<'a>, f : ref<'a> -> State -> 'b) =
            fun (filter, refCtx : RefContext) ->
                 let r = refCtx.New nr.initial
                 f r (filter, refCtx)


        member x.While(guard : unit -> astream<bool>, body : unit -> State -> astream<Result<'a, 'b>>) : astream<Result<'a, 'b>> =
            body () (guard(), RefContext())

    let astream = StreamBuilder()
    let mstream = MagicStreamBuilder()

    let test (input : astream<V2d>) (active : astream<bool>) =
        astream {
            let mutable isActive = false
            let mutable current = []

            for i in active, input do
                match i with
                    | Left v ->
                        if isActive && not v then
                            return current
                            yield []
                            current <- []
                        isActive <- v

                    | Right value ->
                        if isActive then
                            current <- value::current
                            yield current
        }


    let sref v =  { initial = v }

    let testDesired (input : astream<V2d>) (active : astream<bool>) =
        mstream {
            while active do
                let! current = sref []
                for i in input do
                    current := i::!current
                    yield !current

                return !current
        }


    let foldWhile (active : astream<bool>) (f : 's -> 'a -> 's) (seed : 's) (input : astream<'a>)  =
        astream {
            let mutable isActive = false
            let mutable current = seed

            for i in AStream.intersperse active input do
                match i with
                    | Left v ->
                        if isActive && not v then
                            return current
                            current <- seed
                        isActive <- v

                    | Right value ->
                        if isActive then
                            current <- f current value
                            yield current
        }


    let toWorkflow (stream : astream<Result<'a, 'b>>) =
        let intermediate = 
            stream 
                |> AStream.choose (fun e ->
                    match e with
                        | Intermediate v -> Some v
                        | _ -> None
                )
                |> AStream.latest
        
        let final =
            stream
                |> AStream.choose (fun e ->
                    match e with 
                        | Final v -> Some v
                        | _ -> None
                )

        { intermediate = intermediate; final = final }

    let run() =
        let active = CStream.empty
        let positions = CStream.empty

        let result = testDesired positions active |> toWorkflow

        let final = result.final |> AStream.all
        let r = final.GetReader()
        let current = result.intermediate |> Mod.map (function Some m -> m | None -> [])


        current |> Mod.force  |> printfn "current: %A"
        r.Update(); r.Content |> Seq.toList |> printfn "final: %A"


        transact (fun () ->
            CStream.push true active
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
            CStream.push true active
            CStream.push V2d.IO positions
            CStream.push false active
            CStream.push V2d.II positions
            CStream.push V2d.II positions
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