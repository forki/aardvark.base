﻿namespace Aardvark.Base.Incremental


open System
open System.Threading
open System.Threading.Tasks
open System.Collections
open System.Collections.Generic
open System.Collections.Concurrent
open System.Runtime.CompilerServices
open Aardvark.Base


type Change<'a> = list<Delta<'a>>



/// <summary>
/// IReader is the base interface for all adaptive set-readers.
/// Readers are stateful and may not be used by multiple callers
/// since pulling changes mutates the reader internally.
/// </summary>
type IReader<'a> =
    inherit IDisposable
    inherit IAdaptiveObject

    /// <summary>
    /// The reader's current content. 
    /// All changes returned by GetDelta are "relative" to that state.
    /// NOTE that calling GetDelta modifies the reader's content
    /// </summary>
    abstract member Content : ReferenceCountingSet<'a>

    /// <summary>
    /// Update brings the reader's content up-to date without
    /// calculating deltas. 
    /// All other effects are equal to those caused by calling GetDelta
    /// NOTE that Update will cause subsequent calls of GetDelta
    ///      to return empty deltas (as they are always relative to the current content)
    /// For some readers Update can be implemented more efficiently than GetDelta
    /// and in some scenarios the real deltas are not needed.
    /// </summary>
    abstract member Update : IAdaptiveObject -> unit

    /// <summary>
    /// Pulls the reader's deltas relative to its current content.
    /// NOTE that GetDelta also "applies" the deltas to the reader's state
    /// </summary>
    abstract member GetDelta : IAdaptiveObject -> Change<'a>

    abstract member SubscribeOnEvaluate : (Change<'a> -> unit) -> IDisposable




/// <summary>
/// aset serves as the base interface for all adaptive sets.
/// </summary>
[<CompiledName("IAdaptiveSet")>]
type aset<'a> =
    /// <summary>
    /// Returns a NEW reader for the set which will initially return
    /// the entire set-content as deltas.
    /// </summary>
    abstract member Copy : aset<'a>

    abstract member GetReader : unit -> IReader<'a>

    abstract member IsConstant : bool

    abstract member ReaderCount : int

/// <summary>
/// ASetReaders contains implementations of IReader&lt;a&gt; representing
/// the available combinators and is used by the aset-system internally (hence private)
/// </summary>
module ASetReaders =
    let private OneShotReaderEvaluateProbe = Symbol.Create "[ASet] oneshot eval"
    let private ReaderEvaluateProbe = Symbol.Create "[ASet] evaluation"
    let private ReaderComputeProbe = Symbol.Create "[ASet] compute"
    let private ReaderCallbackProbe = Symbol.Create "[ASet] eval callback"
    let private ApplyDeltaProbe = Symbol.Create "[ASet] apply deltas"

    // TODO: efficiency?
    let compareSets (l : ISet<'a>) (r : ISet<'a>) =
        let add = r |> Seq.filter (not << l.Contains) |> Seq.map Add
        let rem = l |> Seq.filter (not << r.Contains) |> Seq.map Rem
        Seq.append add rem |> Seq.toList

    let apply (set : ReferenceCountingSet<'a>) (deltas : list<Delta<'a>>) =
        Telemetry.timed ApplyDeltaProbe (fun () ->
            set.Apply deltas
//            deltas 
//                |> Delta.clean 
//                |> List.filter (fun d ->
//                    match d with
//                        | Add v -> set.Add v
//                        | Rem v -> set.Remove v
//                   )
        )

    [<AbstractClass>]
    type AbstractReader<'a>() =
        inherit AdaptiveObject()



        let mutable isDisposed = 0
        let content = ReferenceCountingSet<'a>()
        let mutable callbacks : HashSet<Change<'a> -> unit> = null


        abstract member Release : unit -> unit
        abstract member ComputeDelta : unit -> Change<'a>
        abstract member Update : IAdaptiveObject -> unit

        member x.Content = content
        member internal x.Callbacks = callbacks

        member x.GetDelta(caller) =
            x.EvaluateIfNeeded caller [] (fun () ->
                Telemetry.timed ReaderEvaluateProbe (fun () ->
                    let deltas = Telemetry.timed ReaderComputeProbe x.ComputeDelta
                    let finalDeltas = Telemetry.timed ApplyDeltaProbe (fun () -> deltas |> apply content)

                    if not (isNull callbacks) then
                        Telemetry.timed ReaderCallbackProbe (fun () ->
                            if not (List.isEmpty finalDeltas) then
                                for cb in callbacks do cb finalDeltas
                        )

                    finalDeltas
                )
            )

        default x.Update(caller) = x.GetDelta(caller) |> ignore

        member x.Dispose disposing =
            if Interlocked.Exchange(&isDisposed,1) = 0 then
                x.Release()
                content.Clear()


        override x.Finalize() =
            try x.Dispose false
            with _ -> ()

        member x.Dispose() =
            x.Dispose true
            GC.SuppressFinalize x

        member x.SubscribeOnEvaluate (cb : Change<'a> -> unit) =
            goodLock123 callbacks (fun () ->
                if isNull callbacks then
                    callbacks <- HashSet()

                if callbacks.Add cb then
                    { new IDisposable with 
                        member __.Dispose() = 
                            goodLock123 callbacks (fun () ->
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

        interface IReader<'a> with
            member x.Content = content
            member x.Update(caller) = x.Update(caller)
            member x.GetDelta(caller) = x.GetDelta(caller)
            member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb

    type UnionReader<'a>(readers : list<IReader<'a>>) as this =
        inherit AbstractReader<'a>()

        let dirty = MutableVolatileDirtySet<IReader<'a>, list<Delta<'a>>>(fun r -> r.GetDelta(this))
        
        let mutable initial = true

        override x.InputChanged(i : IAdaptiveObject) =
            match i with
                | :? IReader<'a> as r -> dirty.Push r
                | _ -> ()

        override x.Inputs =
            readers |> Seq.cast

        override x.ComputeDelta() =
            if initial then 
                initial <- false
                readers |> List.collect (fun r -> r.GetDelta x)
            else 
                dirty.Evaluate() |> List.concat

        override x.Release() =
            dirty.Clear()

    type SmallUnionReader<'a>(readers : list<IReader<'a>>)  =
        inherit AbstractReader<'a>()

        override x.Inputs =
            readers |> Seq.cast

        override x.ComputeDelta() =
            readers |> List.collect (fun r -> r.GetDelta x)

        override x.Release() =
            ()

    type BindReader<'a, 'b>(scope : Ag.Scope, m : IMod<'a>, f : 'a -> IReader<'b>) =
        inherit AbstractReader<'b>()

        let f v = Ag.useScope scope (fun () -> f v)
        let hasChanged = ChangeTracker.track<'a>
        let mutable current : Option<IReader<'b>> = None
        let mutable mChanged = true

        override x.InputChanged(o : IAdaptiveObject) =
            mChanged <- Object.ReferenceEquals(o, m)

        override x.Inputs =
            seq {
                yield m :> _
                match current with 
                    | Some o -> yield o :> _ 
                    | _ -> ()
            }

        override x.Release() =
            match current with
                | Some c -> 
                    c.Dispose()
                    current <- None
                | _ -> ()

        override x.ComputeDelta() =
            let moutdated = mChanged
            mChanged <- false
            
            match current with
                | Some old ->
                    if moutdated then
                        let v = m.GetValue x
                        if hasChanged v then
                            let removal = old.Content |> Seq.map Rem |> Seq.toList
                            old.Dispose()
                            let r = f v 
                            current <- Some r
                            removal @ r.GetDelta x
                        else
                            old.GetDelta x
                    else
                        old.GetDelta x
                | None ->
                    let v = m.GetValue x
                    v |> hasChanged |> ignore
                    let r = f v
                    current <- Some r
                    r.GetDelta x
       
    type BindTaskReader<'a, 'b>(scope : Ag.Scope, m : System.Threading.Tasks.Task<'a>, f : 'a -> IReader<'b>) as this =
        inherit AbstractReader<'b>()

        let mutable f = fun v -> Ag.useScope scope (fun () -> f v)
        let mutable current : Option<IReader<'b>> = None
        let mutable completed = false
        let mutable awaiter = m.GetAwaiter()

        do 
            if m.IsCompleted then
                completed <- true
            else
                awaiter.OnCompleted(fun () -> 
                    completed <- true
                    transact (fun () -> this.MarkOutdated())
                )

        override x.Inputs =
            seq {
                match current with 
                    | Some o -> yield o :> _ 
                    | _ -> ()
            }

        override x.Release() =
            
            match current with
                | Some c -> 
                    c.Dispose()
                    current <- None
                | _ -> ()

        override x.ComputeDelta() =
            if completed then
                let r = 
                    match current with
                        | Some c -> c
                        | None ->
                            let v = awaiter.GetResult()
                            let c = f v
                            current <- Some c
                            awaiter <- Unchecked.defaultof<_>
                            f <- Unchecked.defaultof<_>
                            c

                r.GetDelta x
            else
                []
           
                
    type MultiConstantCollectReader<'a, 'b>(scope : Ag.Scope, sources : seq<IReader<'a>>, f : 'a -> seq<'b>) as this =
        inherit AbstractReader<'b>()
        let sources = Seq.toList sources

        let mutable initial = true
        let cache = Cache(scope, f >> Seq.toList)
        let dirty = MutableVolatileDirtySet<IReader<'a>, list<Delta<'a>>>(fun r -> r.GetDelta this)

        override x.Inputs =
            sources |> Seq.cast

        override x.InputChanged(i : IAdaptiveObject) =
            match i with
                | :? IReader<'a> as r -> dirty.Push r
                | _ -> ()

        override x.Release() =
            for s in sources do s.Dispose()
            cache.Clear ignore
            dirty.Clear()

        override x.ComputeDelta() =
            if initial then
                initial <- false
                sources |> List.collect (fun r ->
                    r.GetDelta x 
                        |> List.collect (fun d ->
                            match d with
                                | Add v -> cache.Invoke v |> List.map Add
                                | Rem v -> cache.Revoke v |> List.map Rem
                        )
                )
            else
                dirty.Evaluate() 
                    |> List.concat
                    |> List.collect (fun d ->
                        match d with
                            | Add v -> cache.Invoke v |> List.map Add
                            | Rem v -> cache.Revoke v |> List.map Rem
                    )

    type ConstantCollectReader<'a, 'b>(scope, source : IReader<'a>, f : 'a -> seq<'b>) =
        inherit AbstractReader<'b>()

        let cache = Cache(scope, f >> Seq.toList)

        override x.Inputs =
            Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            source.Dispose()
            cache.Clear ignore

        override x.ComputeDelta() =
            source.GetDelta x
                |> List.collect (fun v ->
                    match v with
                        | Add v -> cache.Invoke v |> List.map Add
                        | Rem v -> cache.Revoke v |> List.map Rem
                )


    /// <summary>
    /// A reader representing "map" operations
    /// NOTE that the reader actually takes a function "a -> list&lt;b&gt;" instead of the
    ///      usual "a -> b" since it is convenient for some use-cases and does not make 
    ///      the implementation harder.
    /// </summary>
    type MapReader<'a, 'b>(scope, source : IReader<'a>, f : 'a -> list<'b>) =
        inherit AbstractReader<'b>() 
        
        let f = Cache(scope, f)

        override x.Inputs = Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            source.RemoveOutput x
            source.Dispose()
            f.Clear(ignore)

        override x.ComputeDelta() =
            source.GetDelta x 
                |> List.collect (fun d ->
                    match d with
                        | Add v -> f.Invoke v |> List.map Add
                        | Rem v -> f.Revoke v |> List.map Rem
                )

    type MapMReader<'a, 'b>(scope, source : IReader<'a>, f : 'a -> IMod<'b>) =
        inherit AbstractReader<'b>() 
        
        let f = Cache(scope, fun v -> f v)
        let oldValues = Dictionary<IMod<'b>, 'b>()
        let dirty = List<IMod<'b>>()

        member private x.EvalMod (m : IMod<'b>) =
            Locking.read m (fun () ->
                if m.OutOfDate then
                    let n = m.GetValue x
                    match oldValues.TryGetValue m with
                        | (true, old) -> 
                            if System.Object.Equals(old, n) then
                                []
                            else
                                oldValues.[m] <- n
                                [Rem old; Add n]
                        | _ ->
                            [Add n]
                            
                else
                    []
            )


        override x.InputChanged(o : IAdaptiveObject) =
            match o with
                | :? IMod<'b> as o ->
                    goodLock123 dirty (fun () -> dirty.Add o)
                | _ -> ()

        override x.Inputs = Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            source.RemoveOutput x
            source.Dispose()
            f.Clear(ignore)
            oldValues.Clear()

        override x.ComputeDelta() =
            let outer = 
                source.GetDelta x 
                    |> List.collect (fun d ->
                    
                        match d with
                            | Add v -> x.EvalMod(f.Invoke v)
                            | Rem v -> 
                                let m = f.Revoke v
                                match oldValues.TryGetValue m with
                                    | (true, old) ->
                                        oldValues.Remove(m) |> ignore
                                        [Rem old]
                                    | _ ->
                                        []
                    )  

            let dirty = 
                goodLock123 dirty (fun () ->
                    let arr = dirty |> CSharpList.toList
                    dirty.Clear()
                    arr
                )

            let inner =
                dirty |> List.collect x.EvalMod

            outer @ inner

     
    type MapModReader<'a, 'b, 'c>(scope, r : IReader<'a>, m  : IMod<'b>, f : 'a -> 'b -> 'c) =
        inherit AbstractReader<'c>() 

        let mutable modChanged = true
        let mutable currentB = None
        let cache = Cache(fun a -> f a currentB.Value)

        override x.InputChanged(o : IAdaptiveObject) =
            if o.Id = m.Id then modChanged <- true


        override x.Inputs = seq { yield r :> IAdaptiveObject; yield m :> IAdaptiveObject; }

        override x.Release() =
            cache.Clear ignore
            r.RemoveOutput x
            m.RemoveOutput x
            currentB <- None

        override x.ComputeDelta() =
            let replaySetChanges () =
                Ag.useScope scope (fun () ->
                    r.GetDelta x |> List.map (fun d ->
                        match d with
                            | Add x -> 
                                cache.Invoke x |> Add
                            | Rem x ->
                                cache.Revoke x |> Rem
                    )
                )

            if modChanged then
                let b = m.GetValue x
                modChanged <- false
                match currentB with
                 | Some v when System.Object.Equals(v,b) ->
                    replaySetChanges ()
                 | _ ->
                    currentB <- Some b
                    r.GetDelta x |> ignore

                    cache.Clear ignore
                    
                    let newContent = 
                        Ag.useScope scope (fun () ->
                            r.Content |> Seq.map cache.Invoke |> HashSet.ofSeq
                        )

                    compareSets x.Content newContent

            else
                replaySetChanges ()

          
    /// <summary>
    /// A reader representing "collect" operations
    /// NOTE that this is THE core implementation of the entire aset-system and every 
    ///      other reader could be simulated using this one.
    /// </summary>   
    type CollectReader<'a, 'b>(scope, source : IReader<'a>, f : 'a -> IReader<'b>) as this =
        inherit AbstractReader<'b>()

        let f = Cache(scope, f)
        let dirtyInner = MutableVolatileDirtySet(fun (r : IReader<'b>) -> r.GetDelta(this))

        override x.Inputs =
            seq {
                yield source :> IAdaptiveObject
                for c in f.Values do yield c :> IAdaptiveObject
            }


        override x.InputChanged (o : IAdaptiveObject) = 
            match o with
                | :? IReader<'b> as o -> dirtyInner.Add o
                | _ -> ()


        override x.Release() =
            source.RemoveOutput x
            source.Dispose()
            f.Clear(fun r -> r.RemoveOutput x; r.Dispose())
            dirtyInner.Clear()

        override x.ComputeDelta() =
            let xs = source.GetDelta x
            let outerDeltas =
                xs |> List.collect (fun d ->
                    match d with
                        | Add v ->
                            let r = f.Invoke v

                            // we're an output of the new reader
                            // bring the reader's content up-to-date by calling GetDelta
                            r.GetDelta x |> ignore

                            // listen to marking of r (reader cannot be OutOfDate due to GetDelta above)
                            dirtyInner.Add r
                                    
                            // since the entire reader is new we add its content
                            // which must be up-to-date here (due to calling GetDelta above)
                            r.Content |> Seq.map Add |> Seq.toList

                        | Rem v ->
                            let (last, r) = f.RevokeAndGetDeleted v

                            // remove the reader from the listen-set
                            dirtyInner.Remove r

                            // since the reader is no longer contained we don't want
                            // to be notified anymore
                            r.RemoveOutput x
   
                            // the entire content of the reader is removed
                            // Note that the content here might be OutOfDate
                            // TODO: think about implications here when we do not "own" the reader
                            //       exclusively
                            let res = r.Content |> Seq.map Rem |> Seq.toList

                            // if the reader's reference count got 0 we dispose it 
                            // since no one can ever reference it again
                            if last then r.Dispose()

                            res
                )

            // all dirty inner readers must be registered 
            // in dirtyInner. Even if the outer set did not change we
            // need to collect those inner deltas.
            let innerDeltas = dirtyInner.Evaluate() |> List.concat

            // concat inner and outer deltas 
            List.append outerDeltas innerDeltas


    /// <summary>
    /// A reader representing "choose" operations
    /// </summary>   
    type ChooseReader<'a, 'b>(scope, source : IReader<'a>, f : 'a -> Option<'b>) =
        inherit AbstractReader<'b>()

        let f = Cache(scope, f)

        override x.Inputs = Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            source.RemoveOutput x
            source.Dispose()
            f.Clear(ignore)

        override x.ComputeDelta() =
            let xs = source.GetDelta x
            xs |> List.choose (fun d ->
                match d with
                    | Add v ->
                        let r = f.Invoke v

                        match r with
                            | Some r -> Some (Add r)
                            | None -> None

                    | Rem v ->
                        let r = f.Revoke v

                        match r with
                            | Some r -> Some (Rem r)
                            | None -> None

            )

    /// <summary>
    /// A reader for using IMod&lt;a&gt; as a single-valued-set
    /// </summary>   
    type ModReader<'a>(source : IMod<'a>) =  
        inherit AbstractReader<'a>()
        let hasChanged = ChangeTracker.track<'a>
        let mutable old = None

        member x.Source = source

        override x.Inputs = Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            source.RemoveOutput x
            old <- None

        override x.ComputeDelta() =
            let v = source.GetValue(x)
            if hasChanged v then
                match old with
                    | Some c -> 
                        old <- Some v
                        [Rem c; Add v]
                    | None ->
                        old <- Some v
                        [Add v]
            else
                []

    /// <summary>
    /// A reader which allows changes to be pushed from the outside (e.g. cset)
    /// NOTE that atm. EmitReader may keep very long histories since the code fixing that
    ///      is mostly untested and will be "activated" on demand (if someone needs it)
    /// NOTE that it is safe to call Emit from various threads since it is synchronized internally
    /// </summary>   
    type EmitReader<'a>(lockObj : obj, dispose : EmitReader<'a> -> unit) =
        inherit AbstractReader<'a>()

        let deltas = List<Delta<'a>>()
        let mutable reset : Option<ISet<'a>> = None

        override x.Inputs : seq<IAdaptiveObject> = Seq.empty

        member x.Emit (c : ISet<'a>, d : Option<list<Delta<'a>>>) =
            Locking.write x (fun () ->
                match reset with
                    | Some r ->
                        reset <- Some c
                    | None -> 
                        match d with 
                            | Some d ->
                                deltas.AddRange d
        //                        let N = c.Count
        //                        let M = content.Count
        //                        let D = deltas.Count + (List.length d)
        //                        if D > N + 2 * M then
        //                            reset <- Some c
        //                            deltas.Clear()
        //                        else
        //                            deltas.AddRange d

                            | None ->
                                reset <- Some c
                                deltas.Clear()

                if not x.OutOfDate then
                    match getCurrentTransaction() with
                        | Some t ->
                            t.Enqueue x
                        | _ ->
                            failwith "[EmitReader] cannot emit without transaction"
            )

        override x.Release() =
            dispose x
            deltas.Clear()
            reset <- None

        override x.ComputeDelta() =
            let content = x.Content
            match reset with
                | Some c ->
                    goodLock123 lockObj (fun () ->
                        //Interlocked.Increment(&resetCount) |> ignore
                        reset <- None
                        compareSets content c
                    )
                | None ->
                    //Interlocked.Increment(&incrementalCount) |> ignore
                    let res = deltas |> Seq.toList
                    deltas.Clear()
                    res


    type ReferenceCountedReader<'a>(newReader : unit -> IReader<'a>) =
        static let noNewReader : unit -> IReader<'a> = 
            fun () -> failwith "[ASet] implementation claimed that no new readers would be allocated"

        let lockObj = obj()
        let mutable newReader = newReader
        let mutable reader = None
        let mutable refCount = 0
        let containgSetDied = EventSourceSlim ()
        let onlyReader = EventSourceSlim true

        member x.ContainingSetDied() =
            goodLock123 lockObj (fun () ->
                containgSetDied.Emit ()
                //newReader <- noNewReader
                
            )

        member x.OnlyReader = onlyReader :> IEvent<bool>

        member x.ReferenceCount = 
            goodLock123 lockObj (fun () -> refCount)

        member x.ContainingSetDiedEvent = containgSetDied :> IEvent<_>

        member x.GetReference() =
            goodLock123 lockObj (fun () ->
                let reader = 
                    match reader with
                        | None ->
                            let r = newReader()
                            reader <- Some r
                            r
                        | Some r -> r

                refCount <- refCount + 1
                if refCount = 1 then onlyReader.Emit(true)
                else onlyReader.Emit(false)
                reader
            )

        member x.RemoveReference() =
            goodLock123 lockObj (fun () ->
                refCount <- refCount - 1

                if refCount = 1 then onlyReader.Emit(true)
                elif refCount = 0 then
                    reader.Value.Dispose()
                    reader <- None
            )

        override x.ToString() =
            match reader with 
             | Some r -> 
                if r.OutOfDate then r.Content |> Seq.toArray |> sprintf "{ content = %A (outOfDate) }"
                else r.Content |> Seq.toArray |> sprintf "{ content = %A }"
             | None -> sprintf "{ unevaluated aset }"



    type CopyReader<'a>(input : ReferenceCountedReader<'a>) as this =
        inherit AdaptiveObject()
          
        let inputReader = input.GetReference()

        let mutable containgSetDead = false
        let mutable initial = true
        let mutable passThru        : bool                          = true
        let mutable deltas          : List<Delta<'a>>               = null
        let mutable reset           : Option<ISet<'a>>              = None 
        let mutable subscription    : IDisposable                   = null
        let mutable content         : ReferenceCountingSet<'a>      = Unchecked.defaultof<_>
        let mutable callbacks       : HashSet<Change<'a> -> unit>   = null

        let emit (d : list<Delta<'a>>) =
            Locking.write this (fun () ->
//                if reset.IsNone then
//                    let N = inputReader.Content.Count
//                    let M = this.Content.Count
//                    let D = deltas.Count + (List.length d)
//                    if D > N + 2 * M then
//                        reset <- Some (inputReader.Content :> _)
//                        deltas.Clear()
//                    else
//                        deltas.AddRange d


                if reset.IsNone then
                    deltas.AddRange d
            
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
                    Aardvark.Base.Log.warn "[ASetReaders.CopyReader] potentially bad emit with: %A" d
            )
 
        let mutable isDisposed = 0
        
        //let mutable deadSubscription = input.ContainingSetDiedEvent.Values.Subscribe this.ContainingSetDied
        let mutable onlySubscription = 
            let weakThis = Weak this 
            input.OnlyReader.Values.Subscribe(fun pass -> 
                match weakThis.TargetOption with
                    | Some this -> this.SetPassThru(pass, passThru)
                    | _ -> ()
            )


        member x.Inner = inputReader
            

        member x.SetPassThru(active : bool, copyContent : bool) =
            Locking.read inputReader (fun () ->
                Locking.write this (fun () ->
                    if active <> passThru then
                        passThru <- active
                        if passThru then
                            deltas <- null
                            reset <- None
                            subscription.Dispose()
                            subscription <- null
                            content <- Unchecked.defaultof<_>
                        else
                            deltas <- List()
                            content <- ReferenceCountingSet()
                            if copyContent then
                                reset <- None
                                content.SetTo(inputReader.Content)
                            else
                                reset <- Some (inputReader.Content :> ISet<_>)
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

        member x.Content = 
            Locking.read x (fun () ->
                if passThru then
                    inputReader.Content
                else
                    content
            )

        member x.ComputeDelta() =
            if passThru then
                if initial then
                    inputReader.Update x
                    inputReader.Content |> Seq.map Add |> Seq.toList
                else
                    inputReader.GetDelta x

            else
                inputReader.Update x
                inputReader.Outputs.Add x |> ignore

                match reset with
                    | Some c ->
                        reset <- None
                        deltas.Clear()
                        let add = c |> Seq.filter (not << content.Contains) |> Seq.map Add
                        let rem = content |> Seq.filter (not << c.Contains) |> Seq.map Rem

                        Telemetry.timed ApplyDeltaProbe (fun () -> Seq.append add rem |> Seq.toList |> apply content)
                    | None ->
                        let res = deltas |> Seq.toList
                        deltas.Clear()
                        Telemetry.timed ApplyDeltaProbe (fun () -> res |> apply content)

        member x.GetDelta(caller) =
            Locking.read inputReader (fun () ->
                x.EvaluateIfNeeded caller [] (fun () ->
                    Telemetry.timed ReaderEvaluateProbe (fun () ->
                        let deltas = Telemetry.timed ReaderComputeProbe x.ComputeDelta
          
                        if not (isNull callbacks) then
                            Telemetry.timed ReaderCallbackProbe (fun () ->
                                if not (List.isEmpty deltas) then
                                    for cb in callbacks do cb deltas
                            )

                        initial <- false
                        deltas
                    )
                )
            )

        member x.Update(caller) = x.GetDelta(caller) |> ignore

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
                    content.Clear()


                input.RemoveReference() 

        member x.Dispose() =
            x.Dispose true
            GC.SuppressFinalize x

        member x.SubscribeOnEvaluate (cb : Change<'a> -> unit) =
            goodLock123 callbacks (fun () ->
                if isNull callbacks then
                    callbacks <- HashSet()

                if callbacks.Add cb then
                    { new IDisposable with 
                        member __.Dispose() = 
                            goodLock123 callbacks (fun () ->
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

        interface IReader<'a> with
            member x.Content = x.Content
            member x.Update(caller) = x.Update(caller)
            member x.GetDelta(caller) = x.GetDelta(caller)
            member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb


    type OneShotReader<'a>(content : ReferenceCountingSet<'a>) =  
        inherit ConstantObject()
        let mutable callbacks : HashSet<list<Delta<'a>> -> unit> = null
        static let empty = ReferenceCountingSet<'a>()

        let toDeltaList (set : ReferenceCountingSet<'a>) =
            match set.Count with
                | 0 -> []
                | 1 -> [Add (set.FirstOrDefault(Unchecked.defaultof<_>))]
                | _ -> 
                    set.ToArray(set.Count) |> Array.toList |> List.map Add

        let mutable initial = true

        member x.SubscribeOnEvaluate(cb : list<Delta<'a>> -> unit) =
            goodLock123 callbacks (fun () ->
                if isNull callbacks then
                    callbacks <- HashSet()

                if callbacks.Add cb then
                    { new IDisposable with 
                        member __.Dispose() = 
                            goodLock123 callbacks (fun () ->
                                callbacks.Remove cb |> ignore 
                                if callbacks.Count = 0 then
                                    callbacks <- null
                            )
                    }
                else
                    { new IDisposable with member __.Dispose() = () }
            )
        member x.GetDelta(caller : IAdaptiveObject) =
            Telemetry.timed OneShotReaderEvaluateProbe (fun () ->
                Locking.read x (fun () ->
                    if initial then
                        initial <- false
                        let deltas = toDeltaList content

                        if not (isNull callbacks) then
                            for cb in callbacks do
                                cb deltas
                            callbacks <- null

                        deltas
                    else
                        []
                )
            )

        interface IReader<'a> with
            member x.GetDelta(caller) = x.GetDelta(caller)
            member x.SubscribeOnEvaluate(cb) = x.SubscribeOnEvaluate cb
            member x.Update(caller) = 
                x.GetDelta(caller) |> ignore
            member x.Dispose() = ()
            member x.Content = 
                if initial then empty
                else content

    type UseReader<'a when 'a :> IDisposable>(inputReader : IReader<'a>) =
        inherit AbstractReader<'a>()

        override x.Inputs = Seq.singleton (inputReader :> IAdaptiveObject)

        override x.Release() =
            for c in x.Content do
                try c.Dispose() with _ -> ()
            inputReader.RemoveOutput x
            inputReader.Dispose()

        override x.ComputeDelta() =
            let deltas = inputReader.GetDelta(x)

            for d in deltas do
                match d with
                    | Rem v -> v.Dispose()
                    | _ -> ()

            deltas

    type MapUseReader<'a, 'b when 'b :> IDisposable>(scope, source : IReader<'a>, f : 'a -> list<'b>) =
        inherit AbstractReader<'b>() 
        
        let f = Cache(scope, f)

        override x.Inputs = Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            source.RemoveOutput x
            source.Dispose()
            f.Clear(fun (b : list<'b>) -> b |> List.iter (fun b -> b.Dispose()))

        override x.ComputeDelta() =
            source.GetDelta x 
                |> List.collect (fun d ->
                    match d with
                        | Add v -> 
                            f.Invoke v |> List.map Add
                        | Rem v -> 
                            let last, rem = f.RevokeAndGetDeleted v 
                            
                            if last then
                                rem |> List.iter (fun d -> d.Dispose())
                            
                            rem |> List.map Rem
                )

    

    type AsyncMapReader<'a, 'b>(scope : Ag.Scope, source : IReader<'a>, f : 'a -> Async<'b>) as this =
        inherit AbstractReader<'b>()

        let sem = new SemaphoreSlim(1)
        let cancel = new CancellationTokenSource()
        let mutable inputDeltas = []
        let mutable outdated = 0

        let mutable deltas = []
        let cache = Dictionary<obj, ref<int> * Task<'b> * CancellationTokenSource>()

        let emit (d : Delta<'b>) =
            Interlocked.Change(&deltas, fun deltas -> d::deltas) |> ignore
            transact (fun () -> this.MarkOutdated())

        let start (a : 'a) =
            match cache.TryGetValue a with
                | (true, (cnt,t,c)) -> 
                    cnt := !cnt + 1
                | _ ->
                    let comp = 
                        async {
                            let! v = Ag.useScope scope (fun () -> f a)
                            emit (Add v)
                            return v
                        }

                    let cancel = new CancellationTokenSource()
                    let task = Async.StartAsTask(comp, cancellationToken = cancel.Token)
            
                    cache.[a] <- (ref 1, task, cancel)

        let stop (a : 'a) =
            match cache.TryGetValue a with
                | (true, (cnt,t,c)) -> 
                    let cnt = Interlocked.Decrement(&cnt.contents)
                    if cnt = 0 then
                        c.Cancel()
                        cache.Remove a |> ignore
                        let aw = t.GetAwaiter()
                        aw.OnCompleted (fun () ->
                            if t.IsCanceled then ()
                            elif t.IsFaulted then ()
                            elif t.IsCompleted then
                                emit (Rem t.Result)
                            c.Dispose()
                        )

                | _ ->
                    ()

        let puller =
            async {
                do! Async.SwitchToThreadPool()

                while true do
                    let! _ = Async.AwaitIAsyncResult(sem.WaitAsync())
                    let deltas = EvaluationUtilities.evaluateTopLevel (fun () -> source.GetDelta(this))

                    for d in deltas do
                        match d with
                            | Add v -> start v
                            | Rem v -> stop v

            }

        let task = Async.StartAsTask(puller, cancellationToken = cancel.Token)


        override x.Mark() =
            sem.Release() |> ignore
            true


        override x.ComputeDelta() =
            Interlocked.Exchange(&deltas, [])
             
        override x.Inputs = Seq.singleton (source :> IAdaptiveObject)

        override x.Release() =
            cancel.Cancel()
            source.RemoveOutput x
            for (_,_,c) in cache.Values do
                c.Cancel()
            cache.Clear()
            deltas <- []
            cancel.Dispose()
            sem.Dispose()

    type CustomReader<'a>(scope : Ag.Scope, compute : IReader<'a> -> list<Delta<'a>>) =
        inherit AbstractReader<'a>()

        override x.ComputeDelta() =
            compute x

        override x.Release() =
            ()





    // finally some utility functions reducing "noise" in the code using readers
    let map scope (f : 'a -> 'b) (input : IReader<'a>) =
        new MapReader<_, _>(scope, input, fun c -> [f c]) :> IReader<_>

    let mapAsync scope (f : 'a -> Async<'b>) (input : IReader<'a>) =
        new AsyncMapReader<'a, 'b>(scope, input, f) :> IReader<_>

    let mapM scope (f : 'a -> IMod<'b>) (input : IReader<'a>) =
        new MapMReader<_, _>(scope, input, f) :> IReader<_>

    let mapMod scope (b : IMod<'b>) (f : 'a -> 'b -> 'c) (a : IReader<'a>) =
        new MapModReader<_,_,_>(scope, a, b, f )  :> IReader<_>

    let collect scope (f : 'a -> IReader<'b>) (input : IReader<'a>) =
        new CollectReader<_, _>(scope, input, f) :> IReader<_>

    let collect' scope (f : 'a -> seq<'b>) (input : IReader<'a>) =
        new ConstantCollectReader<_, _>(scope, input, f) :> IReader<_>



    let union (input : list<IReader<'a>>) =
        new UnionReader<_>(input) :> IReader<_>

    let smallUnion (input : list<IReader<'a>>) =
        new SmallUnionReader<_>(input) :> IReader<_>

    let bind scope (f : 'a -> IReader<'b>) (input : IMod<'a>) =
        new BindReader<_,_>(scope, input, f) :> IReader<_>

    let bind2 scope (f : 'a -> 'b -> IReader<'c>) (ma : IMod<'a>)  (mb : IMod<'b>)=
        let tup = Mod.map2 (fun a b -> (a,b)) ma mb
        new BindReader<_,_>(scope, tup,  fun (a,b) -> f a b) :> IReader<_>

    let bindTask scope (f : 'a -> IReader<'b>) (input : System.Threading.Tasks.Task<'a>) =
        new BindTaskReader<_,_>(scope, input, f) :> IReader<_>


    let choose scope (f : 'a -> Option<'b>) (input : IReader<'a>) =
        new ChooseReader<_, _>(scope, input, f) :> IReader<_>

    let ofMod (m : IMod<'a>) =
        new ModReader<_>(m) :> IReader<_>

    let using (r : IReader<'a>) =
        new UseReader<'a>(r) :> IReader<_>

    let mapUsing scope (f : 'a -> 'b) (r : IReader<'a>) =
        new MapUseReader<'a, 'b>(scope, r, fun v -> [f v]) :> IReader<_>