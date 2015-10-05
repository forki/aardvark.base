﻿namespace Aardvark.Base.Incremental

open System
open System.Collections.Generic
open Aardvark.Base

/// <summary>
/// IListReader is the base interface for all adaptive list-readers.
/// Readers are stateful and may not be used by multiple callers
/// since pulling changes mutates the reader internally.
/// </summary>
type IListReader<'a> =
    inherit IDisposable
    inherit IAdaptiveObject

    /// <summary>
    /// The root-time used for all deltas returned by the reader
    /// NOTE that each time is associated with a value except for the 
    ///      root-time itself
    /// </summary>
    abstract member RootTime : IOrder
    
    /// <summary>
    /// The reader's current content. 
    /// All changes returned by GetDelta are "relative" to that state.
    /// NOTE that calling GetDelta modifies the reader's content
    /// </summary>
    abstract member Content : TimeList<'a>

    /// <summary>
    /// Pulls the reader's deltas relative to its current content.
    /// NOTE that GetDelta also "applies" the deltas to the reader's state
    /// </summary>
    abstract member GetDelta : unit -> Change<ISortKey * 'a>

    abstract member Update : unit -> unit

    abstract member SubscribeOnEvaluate : (Change<ISortKey * 'a> -> unit) -> IDisposable

/// <summary>
/// alist serves as the base interface for all adaptive lists.
/// </summary>
[<CompiledName("IAdaptiveList")>]
type alist<'a> =
    /// <summary>
    /// Returns a NEW reader for the list which will initially return
    /// the entire list-content as deltas.
    /// </summary>
    abstract member GetReader : unit -> IListReader<'a>


module AListReaders =

    let apply (set : TimeList<'a>) (deltas : list<Delta<ISortKey * 'a>>) =
        deltas 
            |> Delta.clean 
            |> List.map (fun d ->
                match d with
                    | Add v -> set.Add v
                    | Rem (t,_) -> set.Remove t |> ignore

                d
               )

    [<AbstractClass>]
    type AbstractReader<'a>() =
        inherit AdaptiveObject()

        let content = TimeList<'a>()
        let callbacks = HashSet<Change<ISortKey * 'a> -> unit>()

        abstract member Order : IOrder
        abstract member Release : unit -> unit
        abstract member ComputeDelta : unit -> Change<ISortKey * 'a>
        abstract member Update : unit -> unit        
        abstract member GetDelta : unit -> Change<ISortKey * 'a>

        member x.Content = content
        member x.Callbacks = callbacks

        default x.GetDelta() =
            x.EvaluateIfNeeded [] (fun () ->
                let deltas = x.ComputeDelta()
                let finalDeltas = deltas |> apply content

                for cb in callbacks do cb finalDeltas

                finalDeltas
            )

        default x.Update() = x.GetDelta() |> ignore

        override x.Finalize() =
            try x.Dispose()
            with _ -> ()

        member x.Dispose() =
            x.Release()
            content.Clear()

        member x.SubscribeOnEvaluate (cb : Change<ISortKey * 'a> -> unit) =
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

        interface IListReader<'a> with
            member x.Update() = x.Update()
            member x.RootTime = x.Order
            member x.Content = content
            member x.GetDelta() = x.GetDelta()
            member x.SubscribeOnEvaluate cb = x.SubscribeOnEvaluate cb

    type DirtyListReaderSet<'a>() =
        let subscriptions = Dictionary<IListReader<'a>, unit -> unit>()
        let dirty = HashSet<ISortKey * IListReader<'a>>()
        let occurances = Dictionary<IListReader<'a>, HashSet<ISortKey>>()

        let addOccurance (r : IListReader<'a>) (t : ISortKey) =
            match occurances.TryGetValue r with
                | (true, times) ->
                    times.Add t |> ignore
                    false
                | _ ->
                    let times = HashSet [t]
                    occurances.[r] <- times
                    true

        let removeOccurance (r : IListReader<'a>) (t : ISortKey) =
            match occurances.TryGetValue r with
                | (true, times) ->
                    if times.Remove t then
                        if times.Count = 0 then
                            occurances.Remove r |> ignore
                            true
                        else
                            false
                    else
                        false
                | _ ->
                    false

        let allOccurances (r : IListReader<'a>) =
            match occurances.TryGetValue r with
                | (true, times) -> times :> seq<ISortKey>
                | _ -> Seq.empty

        let start (r : IListReader<'a>) (t : ISortKey) =
            if addOccurance r t then
                // create and register a callback function
                let onMarking () =
                    lock dirty (fun () ->
                        for t in allOccurances r do 
                            dirty.Add(t,r) |> ignore
                            subscriptions.Remove r |> ignore
                    )

                r.MarkingCallbacks.Add onMarking |> ignore

                // store the "subscription" for each reader in order
                // to allow removals of readers
                subscriptions.[r] <- fun () -> r.MarkingCallbacks.Remove onMarking |> ignore

                // if the reader is currently outdated add it to the
                // dirty set immediately
                lock r (fun () -> if r.OutOfDate then dirty.Add (t,r) |> ignore)
                true
            else
                false

        let stop (r : IListReader<'a>) (t : ISortKey) =
            lock dirty (fun () ->
                if removeOccurance r t then
                    // if there exists a subscription we remove and dispose it
                    match subscriptions.TryGetValue r with
                        | (true, d) -> 
                            subscriptions.Remove r |> ignore
                            d()
                        | _ -> ()

                    // if the reader is already in the dirty-set remove it from there too
                    dirty.Remove(t,r) |> ignore
                    true
                else
                    false
            )

        member x.Listen(t : ISortKey, r : IListReader<'a>) = start r t
            
        member x.Destroy(t : ISortKey, r : IListReader<'a>) = stop r t

        member x.GetDeltas() =
            let mine = 
                lock dirty (fun () -> 
                    let arr = dirty |> Seq.toList
                    dirty.Clear()
                    arr
                )
            [ for (t,d) in mine do
                // get deltas for all dirty readers and re-register
                // marking callbacks
                let c = 
                    lock d (fun () ->
                        let res = d.GetDelta()
                        x.Listen(t,d) |> ignore
                        res
                    )

                yield! c |> List.map (fun d -> 
                                match d with
                                    | Add(i,v) -> Add(t,i,v)
                                    | Rem(i,v) -> Rem(t,i,v)
                            )
            ]

        member x.Dispose() =
            occurances.Clear()
            for (KeyValue(_, d)) in subscriptions do d()
            dirty.Clear()
            subscriptions.Clear()

        interface IDisposable with
            member x.Dispose() = x.Dispose()
 

    type MapReader<'a, 'b>(scope, f : 'a -> 'b, input : IListReader<'a>) as this =
        inherit AbstractReader<'b>()
        do input.AddOutput this
        
        let f = Cache<'a, 'b>(scope, f)

        override x.Order = input.RootTime

        override x.Release() =
            input.RemoveOutput this
            input.Dispose()
            f.Clear(ignore)

        override x.ComputeDelta() =
            let deltas = input.GetDelta()
            deltas |> List.map (fun d ->
                match d with
                    | Add(t, v) -> Add (t, f.Invoke v)
                    | Rem(t, v) -> Rem (t, f.Revoke v)
            )

    type MapKeyReader<'a, 'b>(scope, f : ISortKey -> 'a -> 'b, input : IListReader<'a>) as this =
        inherit AbstractReader<'b>()
        do input.AddOutput this
        
        let f = Cache<ISortKey * 'a, 'b>(scope, fun (k,v) -> f k v)

        override x.Order = input.RootTime

        override x.Release() =
            input.RemoveOutput this
            input.Dispose()
            f.Clear(ignore)

        override x.ComputeDelta() =
            let deltas = input.GetDelta() 
            deltas |> List.map (fun d ->
                match d with
                    | Add(t, v) -> Add (t, f.Invoke (t,v))
                    | Rem(t, v) -> Rem (t, f.Revoke (t,v))
            )



    type CollectReader<'a, 'b>(scope, input : IListReader<'a>, f : 'a -> IListReader<'b>) as this =
        inherit AbstractReader<'b>()
        do input.AddOutput this

        let dirtyInner = new DirtyListReaderSet<'b>()
        let f = Cache<'a, IListReader<'b>>(scope, f)

        let mutable mapping = NestedOrderMapping()
        let mutable rootTime = mapping.Root

        override x.Release() =
            input.RemoveOutput x
            input.Dispose()
            f.Clear(fun r -> r.RemoveOutput x; r.Dispose())
            dirtyInner.Dispose()
            mapping.Clear()
            rootTime <- Unchecked.defaultof<ISortKey>

        override x.Order = mapping.Order

        override x.ComputeDelta() =
            let xs = input.GetDelta()
                
            let outerDeltas =
                xs |> List.collect (fun d ->
                    match d with
                        | Add (t,v) ->
                            let r = f.Invoke v

                            // bring the reader's content up-to-date by calling GetDelta
                            r.GetDelta() |> ignore

                            // listen to marking of r (reader cannot be OutOfDate due to GetDelta above)
                            if dirtyInner.Listen(t, r) then
                                // we're an output of the new reader
                                r.AddOutput x

                            // since the entire reader is new we add its content
                            // which must be up-to-date here (due to calling GetDelta above)
                            let additions = r.Content |> Seq.map (fun (i,v) -> Add(mapping.Invoke(t, i), v)) |> Seq.toList
                            additions

                        | Rem (t,v) ->
                            let (last, r) = f.RevokeAndGetDeleted v
                                
                            // remove the reader-occurance from the listen-set
                            if dirtyInner.Destroy(t, r) then
                                // since the reader is no longer contained we don't want
                                // to be notified anymore
                                r.RemoveOutput x

                            // the entire content of the reader is removed
                            // Note that the content here might be OutOfDate
                            // TODO: think about implications here when we do not "own" the reader
                            //       exclusively
                            let removals = r.Content |> Seq.map (fun (i,v) -> Rem(mapping.Revoke(t, i), v))  |> Seq.toList

                            // remove all times created for this specific occurance of r
                            mapping.RevokeAll t |> ignore

                            // if the reader's reference count got 0 we dispose it 
                            // since no one can ever reference it again
                            if last then r.Dispose()

                            removals
                )

            // all dirty inner readers must be registered 
            // in dirtyInner. Even if the outer set did not change we
            // need to collect those inner deltas.
            let innerDeltas = 
                dirtyInner.GetDeltas() |> List.map (fun d ->
                    match d with
                        | Add (o,i,v) -> Add(mapping.Invoke(o, i), v)
                        | Rem (o,i,v) -> Rem(mapping.Revoke(o, i), v)
                                
                )

            // concat inner and outer deltas 
            List.append outerDeltas innerDeltas


    type ChooseReader<'a, 'b>(scope, input : IListReader<'a>, f : 'a -> Option<'b>) as this =
        inherit AbstractReader<'b>()
        do input.AddOutput this

        let mapping = OrderMapping()
        let root = mapping.Root
        let f = Cache<'a, Option<'b>>(scope, f)

        override x.Release() =
            input.RemoveOutput this
            input.Dispose()
            mapping.Clear()
            f.Clear(ignore)

        override x.Order = mapping.Order

        override x.ComputeDelta() =
            let deltas = input.GetDelta()

            deltas |> List.choose (fun d ->
                match d with
                    | Add(t, v) -> 
                        match f.Invoke v with
                            | Some v -> 
                                let t' = mapping.Invoke t
                                Add (t', v) |> Some
                            | None -> None
                    | Rem(t, v) -> 
                        match f.Revoke v with
                            | Some v -> 
                                let t' = mapping.Revoke t
                                Rem (t', v) |> Some
                            | None -> None
            )


    type ModReader<'a>(source : IMod<'a>) as this =  
        inherit AbstractReader<'a>()
        do source.AddOutput this
        let mutable old : Option<SimpleOrder.SortKey * 'a> = None
        let mutable order = SimpleOrder.create()
        let hasChanged = ChangeTracker.track<'a>

        override x.Release() =
            source.RemoveOutput x
            match old with
                | Some (t,_) ->
                    order.Delete t
                    old <- None
                | None -> ()


        override x.Order = order :> IOrder

        override x.ComputeDelta() =
            let v = source.GetValue()
                
            if hasChanged v then
                match old with
                    | None ->
                        let t = order.After order.Root //Time.after rootTime
                        old <- Some (t,v)
                        [Add (t :> ISortKey,v)]
                    | Some (t,c) ->
                        let tNew = order.After order.Root
                        order.Delete t
                        old <- Some (tNew, v)
                        [Rem (t :> ISortKey, c); Add (tNew :> ISortKey, v)]
            else
                []


    type SetReader<'a>(input : IReader<'a>) as this =
        inherit AbstractReader<'a>()
        do input.AddOutput this
        
        let order = SimpleOrder.create()
        let times = Dictionary<obj, SimpleOrder.SortKey>()

        override x.Order = order :> IOrder

        override x.Release() =
            input.RemoveOutput x
            order.Clear()
            times.Clear()

        override x.ComputeDelta() =
            let deltas = input.GetDelta()

            deltas |> List.map (fun d ->
                match d with
                    | Add v -> 
                        let t = order.After order.Root.Prev
                        times.Add(d, t)
                        Add(t :> ISortKey, v)
                    | Rem v ->
                        match times.TryGetValue v with
                            | (true, t) ->
                                order.Delete t
                                Rem(t :> ISortKey, v)
                            | _ -> 
                                failwith "removal of unknown value"
            )

    type EmitReader<'a>(lockObj : obj, order : IOrder, dispose : EmitReader<'a> -> unit) =
        inherit AbstractReader<'a>()

        let deltas = List<Delta<ISortKey * 'a>>()
        let mutable reset : Option<TimeList<'a>> = None

        member x.Emit (c : TimeList<'a>, d : Option<list<Delta<ISortKey * 'a>>>) =
            lock x (fun () ->
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

        override x.Order = order

        override x.Release() =
            dispose x
            deltas.Clear()
            reset <- None

        override x.ComputeDelta() =
            let content = x.Content
            match reset with
                | Some c ->
                    lock lockObj (fun () ->
                        //Interlocked.Increment(&resetCount) |> ignore
                        reset <- None
                        let add = c.All |> Seq.filter (not << content.Contains) |> Seq.map Add
                        let rem = content.All |> Seq.filter (not << c.Contains) |> Seq.map Rem

                        Seq.append add rem |> Seq.toList
                    )
                | None ->
                    //Interlocked.Increment(&incrementalCount) |> ignore
                    let res = deltas |> Seq.toList
                    deltas.Clear()
                    res

    type CopyReader<'a>(inputReader : IListReader<'a>, dispose : CopyReader<'a> -> unit) as this =
        inherit AbstractReader<'a>()
        
        let deltas = List()
        let mutable reset = Some inputReader.Content

        let emit (d : list<Delta<ISortKey * 'a>>) =
            lock this (fun () ->
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



        do inputReader.AddOutput this
        let subscription = inputReader.SubscribeOnEvaluate emit

        override x.Order = inputReader.RootTime

        override x.GetDelta() =
            lock inputReader (fun () ->
                x.EvaluateIfNeeded [] (fun () ->
                    let deltas = x.ComputeDelta()
                    let finalDeltas = deltas |> apply x.Content

                    if not (List.isEmpty finalDeltas) then
                        for cb in x.Callbacks do cb finalDeltas

                    finalDeltas
                )
            )

        override x.ComputeDelta() =
            inputReader.Update()

            match reset with
                | Some c ->
                    let content = x.Content
                    reset <- None
                    deltas.Clear()
                    let add = c |> Seq.filter (not << content.Contains) |> Seq.map Add
                    let rem = content |> Seq.filter (not << c.Contains) |> Seq.map Rem

                    Seq.append add rem |> Seq.toList
                | None ->
                    let res = deltas |> Seq.toList
                    deltas.Clear()
                    res

        override x.Release() =
            inputReader.RemoveOutput x
            subscription.Dispose()
            dispose(x)

    type OneShotReader<'a>(order : IOrder, deltas : Change<ISortKey * 'a>) =  
        inherit AbstractReader<'a>()
        
        let mutable order = order
        let mutable deltas = deltas

        override x.Order = order

        override x.ComputeDelta() =
            let res = deltas
            deltas <- []
            order <- Unchecked.defaultof<_>
            res

        override x.Release() =
            deltas <- []
            order <- Unchecked.defaultof<_>

//    type BufferedReader<'a>(rootTime : IOrder, update : unit -> unit, dispose : BufferedReader<'a> -> unit) =
//        inherit AdaptiveObject()
//        let deltas = List()
//        let mutable reset : Option<ICollection<ISortKey * 'a>> = None
//
//        let content = TimeList<'a>()
//
//        
//        let getDeltas() =
//            match reset with
//                | Some c ->
//                    reset <- None
//                    let add = c |> Seq.filter (not << content.Contains) |> Seq.map Add
//                    let rem = content |> Seq.filter (not << c.Contains) |> Seq.map Rem
//
//                    Seq.append add rem |> Seq.toList
//                | None ->
//                    let res = deltas |> Seq.toList
//                    deltas.Clear()
//                    res
//
//        member x.IsIncremental = 
//            true
//
//        member x.Reset(c : ICollection<ISortKey * 'a>) =
//            reset <- Some c
//            deltas.Clear()
//            x.MarkOutdated()
//
//        member x.Emit (d : list<Delta<ISortKey * 'a>>) =
//            deltas.AddRange d
//            x.MarkOutdated()
//
//        new(rootTime, dispose) = new BufferedReader<'a>(rootTime, id, dispose)
//        new(rootTime) = new BufferedReader<'a>(rootTime, id, ignore)
//
//        member x.Dispose() =
//            dispose(x)
//
//        override x.Finalize() = 
//            try x.Dispose()
//            with _ -> ()
//
//        interface IDisposable with
//            member x.Dispose() = x.Dispose()
//
//        interface IListReader<'a> with
//            member x.RootTime = rootTime
//            member x.GetDelta() =
//                update()
//                x.EvaluateIfNeeded [] (fun () ->
//                    let l = getDeltas()
//                    l |> apply content
//                )
//
//            member x.Update() =
//                failwith "dead"
//
//            member x.Content = content
//            member x.SubscribeOnEvaluate cb = failwith "dead"
//
//



    type SortWithReader<'a when 'a : equality>(input : IReader<'a>, cmp : 'a -> 'a -> int) as this =
        inherit AbstractReader<'a>()
        do input.AddOutput this

        let tree = OrderMaintenance<'a>(cmp)
        let root = tree.Root

        override x.Release() =
            input.RemoveOutput this
            input.Dispose()
            tree.Clear()
            
        override x.Order = tree.Order

        override x.ComputeDelta() =
            let deltas = input.GetDelta()

            deltas |> List.map (fun d ->
                match d with
                    | Add v ->
                        let t = tree.Invoke v
                        Add(t, v)
                    | Rem v -> 
                        let t = tree.Revoke v
                        Rem (t, v)
            )


    type ListSetReader<'a>(input : IListReader<'a>) as this =
        inherit ASetReaders.AbstractReader<'a>()
        do input.AddOutput this

        override x.Release() =
            input.RemoveOutput x

        override x.ComputeDelta() =
            let deltas = input.GetDelta()

            let setDeltas =
                deltas |> List.map (fun d ->
                    match d with
                        | Add (_,v) -> 
                            Add v
                        | Rem (_,v) ->
                            Rem v
                )

            setDeltas |> Delta.clean 



    let map (scope) (f : 'a -> 'b) (input : IListReader<'a>) =
        new MapReader<_, _>(scope, f, input) :> IListReader<_>

    let mapKey (scope) (f : ISortKey -> 'a -> 'b) (input : IListReader<'a>) =
        new MapKeyReader<_, _>(scope, f, input) :> IListReader<_>


    let bind scope (f : 'a -> IListReader<'b>) (input : IMod<'a>) =
        new CollectReader<_,_>(scope, new ModReader<_>(input), f) :> IListReader<_>

    let bind2 scope (f : 'a -> 'b -> IListReader<'c>) (ma : IMod<'a>)  (mb : IMod<'b>)=
        let tup = Mod.map2 (fun a b -> (a,b)) ma mb
        new CollectReader<_,_>(scope, new ModReader<_>(tup),  fun (a,b) -> f a b) :> IListReader<_>

    let collect (scope) (f : 'a -> IListReader<'b>) (input : IListReader<'a>) =
        new CollectReader<_, _>(scope, input, f) :> IListReader<_>

    let choose (scope) (f : 'a -> Option<'b>) (input : IListReader<'a>) =
        new ChooseReader<_, _>(scope, input, f) :> IListReader<_>

    let sortWith (cmp : 'a -> 'a -> int) (input : IReader<'a>) =
        new SortWithReader<'a>(input, cmp) :> IListReader<_>

    let ofMod (m : IMod<'a>) : IListReader<'a> =
        new ModReader<'a>(m) :> IListReader<'a>

    let ofSet (m : aset<'a>) : IListReader<'a> =
        new SetReader<'a>(m.GetReader()) :> IListReader<'a>

    let toSetReader (m : alist<'a>) : IReader<'a> =
        new ListSetReader<'a>(m.GetReader()) :> IReader<'a>