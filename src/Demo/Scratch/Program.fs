﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


open System 
open System.Threading
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Monads.State

module Disposable =
    let empty = { new IDisposable with member x.Dispose() = () }

    let inline dispose (d : IDisposable) = d.Dispose()

    let merge (ds : #seq<IDisposable>) =
        { new IDisposable with
            member x.Dispose() = ds |> Seq.iter dispose
        }

    let delay (r : ref<IDisposable>) =
        { new IDisposable with member x.Dispose() = r.Value.Dispose() }

    let fix (f : ref<IDisposable> -> IDisposable) =
        let r = ref empty
        let d = delay r
        r := f(r)
        d

type Future<'a> =
    | Later of (('a -> unit) -> IDisposable)

module Future =
    
    let bind (f : 'a -> Future<'b>) (m : Future<'a>) =
        match m with
            | Later sa ->
                Later (fun cb ->
                    Disposable.fix (fun d ->
                        sa (fun a ->
                            match f a with
                                | Later sb -> d := sb cb
                        )
                    )
                )

    let map (f : 'a -> 'b) (m : Future<'a>) =
        match m with
            | Later sa ->
                Later (fun cb ->
                    sa (fun a ->
                        a |> f |> cb
                    )
                )

    let nextWith (f : 'a -> 'b) (o : IObservable<'a>) =
        Later (fun cb ->
            Disposable.fix (fun d ->
                o.Subscribe(fun v ->
                    d.Value.Dispose()
                    cb (f v)
                )
            )
        )

    let inline next o = nextWith id o
   
    let merge (futures : list<'a * Future<'a>>) =
        let values = futures |> List.map fst |> List.toArray
        Later (fun cb ->
            Disposable.fix (fun self ->
                futures 
                    |> List.mapi (fun i (_,Later s) ->
                        s (fun v -> 
                            self.Value.Dispose()
                            values.[i] <- v
                            cb (Array.toList values)
                        )
                    ) 
                    |> Disposable.merge
            )
        )

type Cont<'s, 'a> = { run : State<'s, Res<'s, 'a>> }

and Res<'s, 'a> =
    | Cancel
    | Result of 'a
    | Continue of Future<Cont<'s, 'a>>


type Cont<'a> = { runUnit : unit -> Res<'a> }

and Res<'a> =
    | CancelUnit
    | ResultUnit of 'a
    | ContinueUnit of Future<Cont<'a>>



type Bottom = class end

type Pattern<'s, 'a> =
    | Not of Cont<'s, 'a>

module Cont =

    let private fix (f : ref<'a> -> 'a) =
        let r = ref Unchecked.defaultof<'a>
        r := f r
        !r

    let rec withState (m : Cont<'a>) : Cont<'s, 'a> =
        { run =
            state {
                let res = m.runUnit()
                match res with
                    | CancelUnit -> return Cancel
                    | ResultUnit v -> return Result v
                    | ContinueUnit c ->
                        return c |> Future.map withState |> Continue
            }
        }

    let result (v : 'a) : Cont<'s, 'a> =
        { run =
            state {
                return Result v 
            }
        }

    let next (o : IObservable<'a>) : Cont<'s, 'a> =
        { run =
            state {
                return o |> Future.nextWith result |> Continue
            }
        }


    let rec bind (f : 'a -> Cont<'s, 'b>) (m : Cont<'s, 'a>) =
        { run =
            state {
                let! a = m.run
                match a with
                    | Cancel ->
                        return Cancel
                    | Result a ->
                        return! f(a).run
                    | Continue c ->
                        return c |> Future.map (bind f) |> Continue
            }
        }

    let rec map (f : 'a -> 'b) (m : Cont<'s, 'a>) =
        { run =
            m.run |> State.map (fun a ->
                match a with
                    | Cancel ->
                        Cancel

                    | Result a ->
                        a |> f |> Result

                    | Continue c ->
                        c |> Future.map (map f) |> Continue
            )
        }

    let rec append (l : Cont<'s, unit>) (r : Cont<'s, 'a>) =
        { run =
            state {
                let! l = l.run
                match l with
                    | Cancel -> 
                        return Cancel

                    | Result () -> 
                        return! r.run

                    | Continue c ->
                        return c |> Future.map (fun c -> append c r) |> Continue
            }
        }
    
    let rec repeat (m : Cont<'s, unit>) : Cont<'s, Bottom> =
        fix (fun self ->
            { run = 
                state {
                    let! r = m.run
                    match r with
                        | Cancel ->
                            return Cancel

                        | Result () -> 
                            return! repeat(m).run

                        | Continue c ->
                            return c |> Future.map (fun c -> append c !self) |> Continue
                }
            }
        )

    let inline ignore (m : Cont<'s, 'a>) =
        map ignore m


    let delay (f : Future<Cont<'s, 'a>>) =
        { run = 
            state {
                return Continue f
            }
        }

    let rec any (alts : list<Cont<'s, 'a>>) : Cont<'s, 'a> =
        { run =
            state {
                let! res = alts |> List.mapS (fun a -> a.run)
                let mutable result = None
                let futures =
                    res |> List.choose (fun c ->
                        match c with
                            | Cancel -> None
                            | Continue f -> Some f
                            | Result v -> 
                                result <- Some v
                                None
                    )

                match result with
                    | Some v -> 
                        return Result v

                    | None ->
                        let future = futures |> List.map (fun v -> delay v, v) |> Future.merge
                        return future |> Future.map any |> Continue

            }
        }

    let rec guarded (guard : Cont<'s, 'x>) (m : Cont<'s, 'a>) =
        { run =
            state {
                let! g = guard.run
                match g with
                    | Cancel -> 
                        return Cancel

                    | Result _ ->
                        printfn "guard triggered"
                        return Result None

                    | Continue cg ->
                        let! r = m.run
                        match r with
                            | Cancel ->
                                return Cancel
                            | Result a -> 
                                return Result (Some a)
                            | Continue ci ->
                                return ci |> Future.map (guarded guard) |> Continue
            }
        }

    let repeatUntil (guard : Cont<'s, 'x>) (m : Cont<'s, unit>) =
        let cancelOrM = 
            any [
                guard |> map Choice2Of2
                m |> map Choice1Of2
            ]

        let rec repeatUntil (current : Cont<'s, Choice<unit, 'x>>) =
            { run = 
                state {
                    let! r = current.run
                    match r with
                        | Cancel ->
                            return Cancel

                        | Result r ->
                            match r with
                                | Choice1Of2 () -> return! (repeatUntil cancelOrM).run
                                | Choice2Of2 _ -> return Result ()

                        | Continue c ->
                            return c |> Future.map (bind (fun v -> match v with | Choice2Of2 _ -> result () | _ -> repeatUntil cancelOrM)) |> Continue
                }
            }

        repeatUntil cancelOrM


type ContRunner<'s>(state : 's) =
    
    let mutable state = state
    let pending = System.Collections.Concurrent.ConcurrentQueue<Cont<'s, unit>>()
    let sem = new SemaphoreSlim(0)



    member x.Enqueue(c : Cont<'s, 'a>) =
        pending.Enqueue (Cont.ignore c)
        sem.Release() |> ignore

    member x.Start() =
        let run =
            async {
                do! Async.SwitchToNewThread()

                while true do
                    let! _ = sem.WaitAsync() |> Async.AwaitTask
                    match pending.TryDequeue() with
                        | (true, c) ->
                            match c.run.Run(&state) with
                                | Cancel -> ()
                                | Result () -> ()
                                | Continue c ->
                                    match c with
                                        | Later s -> s x.Enqueue |> ignore
                                    
                        | _ ->
                            ()

            }

        Async.Start run



    

[<AutoOpen>]
module ``Cont Builders`` =
    type ContBuilder() =

        member x.Bind(m : IObservable<'a>, f : 'a -> Cont<'s, 'b>) =
            Cont.bind f (Cont.next m)

        member x.Bind(m : Cont<'s, 'a>, f : 'a -> Cont<'s, 'b>) =
            Cont.bind f m

        member x.Return(v : 'a) = 
            Cont.result v

        member x.While(guard : unit -> Pattern<'s, 'a>, body : Cont<'s, unit>) =
            match guard() with
                | Not cancel -> Cont.repeatUntil cancel body

        member x.Delay(f : unit -> Cont<'s, 'a>) =
            { run =
                state {
                    let! r = f().run
                    return r
                }
            }


        member x.Combine(l : Cont<'s, unit>, r : Cont<'s, 'a>) =
            Cont.append l r

        member x.Zero() = Cont.result ()

    let cont = ContBuilder()

type EventList<'a> =
    | Finished of EventList<'a>
    | Empty
    | Snoc of EventList<'a> * DateTime * 'a with

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new EventListEnumerator<'a>(x) :> System.Collections.IEnumerator

    interface System.Collections.Generic.IEnumerable<DateTime * 'a> with
        member x.GetEnumerator() = new EventListEnumerator<'a>(x) :> System.Collections.Generic.IEnumerator<_>

and private EventListEnumerator<'a>(start : EventList<'a>) =
    let stack = Stack<DateTime * 'a>()

    let init(e : EventList<'a>) =
        let rec traverse (e : EventList<'a>) =
            match e with
                | Finished e -> traverse e
                | Empty -> ()
                | Snoc(b,t,v) ->
                    stack.Push(t,v)
                    traverse b
        stack.Clear()
        traverse e
        
    do init start

    member x.MoveNext() =
        if stack.Count > 0 then
            stack.Pop() |> ignore
            true
        else
            false

    member x.Reset() =
        init start

    member x.Current = stack.Peek()

    interface System.Collections.IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
        member x.Current = x.Current :> obj

    interface System.Collections.Generic.IEnumerator<DateTime * 'a> with
        member x.Dispose() = ()
        member x.Current = x.Current

module EventList =

    let empty<'a> : EventList<'a> = Empty

    let rec ofSeq (s : seq<DateTime * 'a>) =
        let mutable res = Empty
        for t,v in s do
            res <- Snoc(res, t, v)

        res
            
    let rec ofList (s : list<DateTime * 'a>) = s |> ofSeq
     
    let snoc (l : EventList<'a>) (t : DateTime) (v : 'a) =
        Snoc(l, t, v)

    let finish (l : EventList<'a>) =
        Finished l

    let rec map (f : 'a -> 'b) (m : EventList<'a>) =
        match m with
            | Finished e -> Finished (map f e)
            | Empty -> Empty
            | Snoc (b, t, v) -> Snoc (map f b, t, f v)

    let rec collect (f : 'a -> #seq<'b>) (m : EventList<'a>) =
        match m with
            | Finished e -> Finished (collect f e)
            | Empty -> Empty
            | Snoc (l, t, v) -> 
                let mutable res = collect f l
                for v in f v do
                    res <- Snoc(res, t, v)
                res

    let rec append (l : EventList<'a>) (r : EventList<'a>) =
        match l with
            | Finished _ -> l
            | Empty -> r
            | _ ->
                match r with
                    | Empty -> l
                    | Snoc(b, t, v) -> Snoc(append l b, t, v)
                    | Finished(r) -> Finished(append l r)

    let rec merge (l : EventList<'a>) (r : EventList<'a>) =
        match l, r with
            | Empty, r -> r
            | l, Empty -> l
            | Finished l, _ -> merge l r
            | _, Finished r -> merge l r
            | Snoc(bl, tl, vl), Snoc(br, tr, vr) ->
                if tl > tr then Snoc(merge bl r, tl, vl)
                else Snoc(merge l br, tr, vr)

    let isEmpty (l : EventList<'a>) =
        match l with
            | Empty -> true
            | _ -> false

    let isFinished (l : EventList<'a>) =
        match l with
            | Finished _ -> true
            | _ -> false

type IStreamReader<'a> =
    inherit IAdaptiveObject
    inherit IDisposable
    abstract member GetEvents : IAdaptiveObject -> EventList<'a>
    abstract member SubscribeOnEvaluate : (EventList<'a> -> unit) -> IDisposable

type astream<'a> =
    abstract member GetReader : unit -> IStreamReader<'a>

module AStream =

    module private System =
        open System.Reactive.Subjects
        open System.Threading

        type ReferenceCountedReader<'a>(newReader : unit -> IStreamReader<'a>) =
            static let noNewReader : unit -> IReader<'a> = 
                fun () -> failwith "[AStream] implementation claimed that no new readers would be allocated"

            let lockObj = obj()
            let mutable newReader = newReader
            let mutable reader = None
            let mutable refCount = 0
            let containgSetDied = new Subject<unit>()
            let onlyReader = new Subject<bool>()

            member x.ContainingSetDied() =
                lock lockObj (fun () ->
                    containgSetDied.OnNext ()
                    //newReader <- noNewReader
                
                )

            member x.OnlyReader = onlyReader :> IObservable<bool>

            member x.ReferenceCount = 
                lock lockObj (fun () -> refCount)

            member x.ContainingSetDiedEvent = containgSetDied :> IObservable<_>

            member x.GetReference() =
                lock lockObj (fun () ->
                    let reader = 
                        match reader with
                            | None ->
                                let r = newReader()
                                reader <- Some r
                                r
                            | Some r -> r

                    refCount <- refCount + 1
                    if refCount = 1 then onlyReader.OnNext(true)
                    else onlyReader.OnNext(false)
                    reader
                )

            member x.RemoveReference() =
                lock lockObj (fun () ->
                    refCount <- refCount - 1

                    if refCount = 1 then onlyReader.OnNext(true)
                    elif refCount = 0 then
                        reader.Value.Dispose()
                        reader <- None
                )

        type CopyReader<'a>(input : ReferenceCountedReader<'a>) as this =
            inherit AdaptiveObject()
          
            let inputReader = input.GetReference()

            let mutable containgSetDead = false
            let mutable initial = true
            let mutable passThru        : bool                                  = true
            let mutable events          : EventList<'a>                         = EventList.empty
            let mutable subscription    : IDisposable                           = null
            let mutable callbacks       : HashSet<EventList<'a> -> unit>        = null

            let emit (d : EventList<'a>) =
                lock this (fun () ->
                    events <- EventList.append events d
            
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
                        Aardvark.Base.Log.warn "[AStreamReaders.CopyReader] potentially bad emit with: %A" d
                )
 
            let mutable isDisposed = 0
        
            //let mutable deadSubscription = input.ContainingSetDiedEvent.Values.Subscribe this.ContainingSetDied
            let mutable onlySubscription = 
                let weakThis = Weak this 
                input.OnlyReader.Subscribe(fun pass -> 
                    match weakThis.TargetOption with
                        | Some this -> this.SetPassThru(pass, passThru)
                        | _ -> ()
                )


            member x.Inner = inputReader
            

            member x.SetPassThru(active : bool, copyContent : bool) =
                lock inputReader (fun () ->
                    lock this (fun () ->
                        if active <> passThru then
                            passThru <- active
                            if passThru then
                                events <- EventList.empty
                                subscription.Dispose()
                                subscription <- null
                            else
                                events <- EventList.empty
                                subscription <- inputReader.SubscribeOnEvaluate(emit)
                                ()
                    )
                )

    //        member private x.ContainingSetDied() =
    //            deadSubscription.Dispose()
    //            if not input.OnlyReader.Latest then
    //                deadSubscription <- input.OnlyReader.Values.Subscribe(fun pass -> if pass then x.ContainingSetDied())
    //            else
    //                containgSetDead <- true
    //                x.Optimize()

            member x.PassThru =
                passThru

            member private x.Optimize() =
                () //Log.line "optimize: input: %A -> %A" inputReader.Inputs inputReader

            override x.Inputs = Seq.singleton (inputReader :> IAdaptiveObject)

            member x.Compute() =
                if passThru then
                    inputReader.GetEvents x

                else
                    inputReader.GetEvents x |> ignore
                    inputReader.Outputs.Add x |> ignore

                    let res = events
                    events <- EventList.empty
                    res

            member x.GetEvents(caller) =
                lock inputReader (fun () ->
                    x.EvaluateIfNeeded caller EventList.empty (fun () ->
                        let deltas = x.Compute()
          
                        if not (isNull callbacks) then
                            if not (EventList.isEmpty events) then
                                for cb in callbacks do cb deltas

                        initial <- false
                        deltas
                    )
                )

            override x.Finalize() =
                try x.Dispose false
                with e -> Report.Warn("finalizer faulted: {0}", e.Message)
            
            member x.Dispose disposing =
                if Interlocked.Exchange(&isDisposed, 1) = 0 then
                    onlySubscription.Dispose()
                    //deadSubscription.Dispose()
                    inputReader.RemoveOutput x
                    if not passThru then
                        subscription.Dispose()


                    input.RemoveReference() 

            member x.Dispose() =
                x.Dispose true
                GC.SuppressFinalize x

            member x.SubscribeOnEvaluate (cb : EventList<'a> -> unit) =
                lock x (fun () ->
                    if isNull callbacks then
                        callbacks <- HashSet()

                    if callbacks.Add cb then
                        { new IDisposable with 
                            member __.Dispose() = 
                                lock x (fun () ->
                                    callbacks.Remove cb |> ignore 
                                    if callbacks.Count = 0 then
                                        callbacks <- null
                                )
                        }
                    else
                        { new IDisposable with member __.Dispose() = () }
                )

            interface IDisposable with
                member x.Dispose() = x.Dispose()

            interface IStreamReader<'a> with
                member x.GetEvents c = x.GetEvents c
                member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb

        type AdaptiveStream<'a>(newReader : unit -> IStreamReader<'a>) =
            let mutable inputReader = ReferenceCountedReader(newReader)

            override x.Finalize() =
                try inputReader.ContainingSetDied()
                with _ -> ()

            override x.ToString() =
                sprintf "%A" inputReader

            interface astream<'a> with
                member x.GetReader () =
                    let reader = new CopyReader<'a>(inputReader)
                    if inputReader.ReferenceCount > 1 then reader.SetPassThru(false, false)

                    reader :> _
 
        let stream (f : unit -> IStreamReader<'a>) =
            AdaptiveStream<'a>(f) :> astream<'a>

    module Readers =
        [<AbstractClass>]
        type AbstractReader<'a>() =
            inherit AdaptiveObject()



            let mutable isDisposed = 0
            let mutable callbacks : HashSet<EventList<'a> -> unit> = null


            abstract member Release : unit -> unit
            abstract member Compute : unit -> EventList<'a>
            abstract member Update : IAdaptiveObject -> unit

            member internal x.Callbacks = callbacks

            member x.GetEvents(caller) =
                if isDisposed = 1 then
                    EventList.empty
                else
                    x.EvaluateIfNeeded caller EventList.empty (fun () ->
                        let events = x.Compute()

                        if not (isNull callbacks) then
                            if not (EventList.isEmpty events) then
                                for cb in callbacks do cb events

                        if EventList.isFinished events then
                            x.Dispose()

                        events
                    )

            default x.Update(caller) = x.GetEvents(caller) |> ignore

            member x.Dispose disposing =
                if Interlocked.Exchange(&isDisposed,1) = 0 then
                    x.Release()
                    let mutable foo = 0
                    x.Outputs.Consume(&foo) |> ignore


            override x.Finalize() =
                try x.Dispose false
                with _ -> ()

            member x.Dispose() =
                x.Dispose true
                GC.SuppressFinalize x

            member x.SubscribeOnEvaluate (cb : EventList<'a> -> unit) =
                lock x (fun () ->
                    if isNull callbacks then
                        callbacks <- HashSet()

                    if callbacks.Add cb then
                        { new IDisposable with 
                            member __.Dispose() = 
                                lock x (fun () ->
                                    callbacks.Remove cb |> ignore 
                                    if callbacks.Count = 0 then
                                        callbacks <- null
                                )
                        }
                    else
                        { new IDisposable with member __.Dispose() = () }
                )

            interface IDisposable with
                member x.Dispose() = x.Dispose()

            interface IStreamReader<'a> with
                member x.GetEvents(caller) = x.GetEvents(caller)
                member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb

        type EmitReader<'a>(remove : EmitReader<'a> -> unit) =
            inherit AbstractReader<'a>()

            let mutable events = EventList.empty

            member x.Emit(t : DateTime, v : 'a) =
                lock x (fun () -> events <- EventList.snoc events t v)
                x.MarkOutdated()

            member x.Finish() =
                lock x (fun () -> events <- EventList.finish events)
                x.MarkOutdated()

            override x.Compute() =
                let res = events
                events <- EventList.empty
                res

            override x.Release() =
                remove x


        type MapReader<'a, 'b>(scope : Ag.Scope, input : IStreamReader<'a>, f : 'a -> list<'b>) =
            inherit AbstractReader<'b>()

            override x.Compute() =
                Ag.useScope scope (fun () ->
                    let evts = input.GetEvents x
                    evts |> EventList.collect f

                )

            override x.Release() =
                input.Dispose()

        type MergeReader<'a>(inputs : list<IStreamReader<'a>>) =
            inherit AbstractReader<'a>()

            let inputs = List.toArray inputs
            let mutable running = inputs.Length

            override x.Compute() =
                let mutable result = EventList.empty
                for r in inputs do
                    let innerEvents = 
                        match r.GetEvents x with
                            | Finished v ->
                                running <- running - 1
                                v
                            | v ->
                                v

                    result <- EventList.merge result innerEvents


                if running = 0 then Finished result
                else result

            override x.Release() =
                inputs |> Array.iter (fun r -> r.Dispose())

        type TakeNReader<'a>(input : IStreamReader<'a>, n : int) =
            inherit AbstractReader<'a>()

            let mutable n = n

            override x.Compute() =
                let mutable result = EventList.empty
                let input = input.GetEvents(x)
                for t,v in input do
                    if n > 0 then
                        result <- EventList.snoc result t v
                        n <- n - 1

                if n = 0 then EventList.finish result
                else result

            override x.Release() =
                input.Dispose()



        let map (scope : Ag.Scope) (f : 'a -> 'b) (input : IStreamReader<'a>) =
            new MapReader<_,_>(scope, input, f >> List.singleton) :> IStreamReader<_>

        let choose (scope : Ag.Scope) (f : 'a -> Option<'b>) (input : IStreamReader<'a>) =
            new MapReader<_,_>(scope, input, f >> Option.toList) :> IStreamReader<_>   

        let merge (l : list<IStreamReader<'a>>) =
            new MergeReader<'a>(l) :> IStreamReader<_>     

        let take (n : int) (s : IStreamReader<'a>) =
            new TakeNReader<'a>(s, n) :> IStreamReader<_> 

    let map (f : 'a -> 'b) (m : astream<'a>) =
        let scope = Ag.getContext()
        System.stream (fun () -> m.GetReader() |> Readers.map scope f)

    let choose (f : 'a -> Option<'b>) (m : astream<'a>) =
        let scope = Ag.getContext()
        System.stream (fun () -> m.GetReader() |> Readers.choose scope f)   

    let merge (streams : #seq<astream<'a>>) =
        System.stream (fun () -> streams |> Seq.toList |> List.map (fun s -> s.GetReader()) |> Readers.merge)   

    let take (n : int) (s : astream<'a>) =
        System.stream (fun () -> s.GetReader() |> Readers.take n)

type InputStream<'a>() =
    let mutable isFinished = 0
    let readers = HashSet<AStream.Readers.EmitReader<'a>>()

    let remove (r : AStream.Readers.EmitReader<'a>) =
        lock readers (fun () ->
            readers.Remove r |> ignore
        )

    member x.GetReader() =
        if isFinished = 0 then
            let r = new AStream.Readers.EmitReader<'a>(remove)
            lock readers (fun () -> readers.Add r |> ignore)
            r :> IStreamReader<_>
        else
            failwithf "[AStream] cannot read finished stream"

    member x.Emit(t : DateTime, v : 'a) =
        if isFinished = 0 then
            lock readers (fun () ->
                for r in readers do
                    r.Emit(t,v)
            )
        else
            failwithf "[AStream] cannot emit to finished stream"
            

    member x.Finish() =
        if Interlocked.Exchange(&isFinished, 1) = 0 then
            lock readers (fun () ->
                for r in readers do
                    r.Finish()

                readers.Clear()
            )     
    interface astream<'a> with
        member x.GetReader() = x.GetReader()





module Urdar =


    type Source = interface end

    type RealSource(subscribe : (obj -> unit) -> IDisposable) =
        interface Source
        member x.Subscribe = subscribe

    type CompositeSource(sources : list<Source>) =
        interface Source
        member x.Sources = sources

    let (|Real|Composite|) (s : Source) =
        match s with
            | :? RealSource as s -> Real s.Subscribe
            | :? CompositeSource as c -> Composite c.Sources 
            | _ -> failwith ""


    type Source<'a>(s : Source) =
        inherit CompositeSource([s])

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Source =
        
        let ofObservable (o : IObservable<'a>) =
            let s (f : obj -> unit) = o.Subscribe(fun a -> f (a :> obj))
            Source(RealSource(s))


    type Occ = Source * obj
    type Occ<'a> = Source<'a> * 'a

    open Aardvark.Base.Monads.State

    type React<'s,'a> =
        | Await of Source * (Occ -> State<'s,React<'s,'a>>)
        | Done of 'a

    type State<'s> =
        {
            waitOn : PersistentHashSet<Source>
            userState : 's
        }

    type Result<'s, 'a> = 
        | Result of 'a
        | Waiting of (Occ -> Option<React3<'s, 'a>>)

    and React3<'s, 'a> = { run : State<State<'s>, Result<'s, 'a>> }






    type React2<'s, 'a> =
        abstract member Bind : ('a -> React2<'s,'b>) -> React2<'s,'b>

    type Done<'s,'a>(v : 'a) =
        interface React2<'s,'a> with
            member x.Bind(f) = f v

    type Await2<'s,'a,'b>(m : Source<'a>, f : 'a -> React2<'s,'b>) =
        interface React2<'s,'b> with
            member x.Bind(f) = 
                let source : RealSource = failwith ""

                


    let AwaitT (s : Source<'a>, f : Occ<'a> -> State<'s, React<'s,'b>>) = 
        let mutable self = Unchecked.defaultof<_>
        self <- Await(s, fun (usource,uvalue) -> 
            if System.Object.Equals(s,usource) then 
                f (s, unbox<'a> uvalue) 
            else State.value self )
        self
        

    type SF<'s,'a> = State<'s,React<'s,'a>>

    let await (o : IObservable<'e>) (f : Occ<'e> -> State<'s,React<'s,'b>>) =
        AwaitT (Source.ofObservable o, f)

    let rec bind (f : 'a -> SF<'s, 'b>) (m : SF<'s, 'a>) : SF<'s, 'b> =
        state {
            let! a = m
            match a with
                | Done value -> return! f value
                | Await(source, cont) -> return Await(source, cont >> bind f)
        }

    let ofSource (o : Source<'a>) : SF<'s, 'a> =
        state {
            let self = ref Unchecked.defaultof<_>
            self := Await (o, fun (source, value) -> 
                state {
                    if Object.Equals(o, source) then
                        return Done (unbox<'a> value)
                    else
                        return !self
                }
            )

            return !self
        }


    let amb (ms : list<'k * SF<'s, 'a>>) : SF<'s, 'k * 'a> =
        state {
            let! bla = ms |> List.mapS (fun (k, sf) -> sf |> State.map (fun sf -> k, sf))
            
            let finished = bla |> List.choose (function (k,Done v) -> Some (k,v) | _ -> None)
            let pending = bla |> List.choose (function (k,Await(source, cont)) -> Some(k,source, cont) | _ -> None)

            match finished with
                | [] ->
                    let any = pending |> List.map (fun (k,s,_) -> k,s) |> AnySource :> Source
                    return Await(any, fun v -> 
                        let (k, value) = unbox<'k * obj> v
                        let value = unbox<'a> value
                        state { return Done(k, value) }
                    )
                | finished :: _ ->
                    return Done finished

        }

    let result r = Done r

       
    let repeatUntil (abortCriterion : SF<'s,'a>) (a : SF<'s,'b>) =
        let! abort = abortCriterion
        match abort with
            | Done e -> 


module Test =
    
    let paintThingy (down : IObservable<_>) (up : IObservable<_>) (move : IObservable<_>) =
        cont {
            let! d = down
            printfn "down"

            let mutable res = []
            while Not (Cont.next up) do
                let! m = move
                printfn "move"
                res <- m :: res
                
            printfn "up"
            return res

        }



open System.Windows.Forms



[<EntryPoint; STAThread>]
let main argv = 
    React.Test.run()
    Environment.Exit 0

    let form = new Form()


    let runner = ContRunner<int>(0)

    let start = form.KeyDown |> Observable.filter (fun e -> e.KeyCode = Keys.Q)
    let stop = form.KeyDown |> Observable.filter (fun e -> e.KeyCode = Keys.W)
    let move = form.KeyDown |> Observable.filter (fun e -> e.KeyCode = Keys.R) |> Observable.map (fun _ -> 1)

    runner.Enqueue (Cont.repeat (Test.paintThingy form.MouseDown form.MouseUp form.MouseMove |> Cont.ignore))

    runner.Start()


    Application.Run form


    //StoreTest.FS.moreTest()
    //StoreTest.FS.test()
    0 // return an integer exit code
