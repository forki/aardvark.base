﻿namespace Aardvark.Base.Incremental

open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Collections.Concurrent
open Aardvark.Base

/// <summary>
/// IMod is the non-generic base interface for 
/// modifiable cells. This is needed due to the
/// lack of existential types in the .NET.
/// </summary>
[<AllowNullLiteral>]
type IMod =
    inherit IAdaptiveObject

    /// <summary>
    /// returns whether or not the cell's content 
    /// will remain constant. 
    /// </summary>
    abstract member IsConstant : bool

    /// <summary>
    /// returns the cell's content and evaluates
    /// the respective computation if needed.
    /// </summary>
    abstract member GetValue : IAdaptiveObject -> obj

/// <summary>
/// IMod<'a> represents the base interface for
/// modifiable cells and provides a method for
/// getting the cell's current content.
/// </summary>
[<AllowNullLiteral>]
type IMod<'a> =
    inherit IMod

    /// <summary>
    /// returns the cell's content and evaluates
    /// the respective computation if needed.
    /// </summary>
    abstract member GetValue : IAdaptiveObject -> 'a

/// <summary>
/// ModRef<'a> represents a changeable input
/// cell which can be changed by the user and
/// implements IMod<'a>
/// </summary>
type IModRef<'a> =
    inherit IMod<'a>

    /// Gets or sets the refs value.
    /// Note: can only be set inside an active transaction.
    abstract member Value : 'a with get,set

/// <summary>
/// ModRef<'a> represents a changeable input
/// cell which can be changed by the user and
/// implements IMod<'a>
/// </summary>
type ModRef<'a>(value : 'a) =
    inherit AdaptiveObject()

    let mutable value = value
    let tracker = ChangeTracker.trackVersion<'a>

    member x.UnsafeCache
        with get() = value
        and set v = value <- v

    member x.Value
        with get() = value
        and set v =
            if tracker v || not <| Object.Equals(v, value) then
                value <- v
                x.MarkOutdated()

    member x.GetValue(caller : IAdaptiveObject) =
        x.EvaluateAlways caller (fun () ->
            value
        )

    interface IMod with
        member x.IsConstant = false
        member x.GetValue(caller) = x.GetValue(caller) :> obj

    interface IMod<'a> with
        member x.GetValue(caller) = x.GetValue(caller)

    interface IModRef<'a> with
        member x.Value 
            with get () = x.Value
            and set v = x.Value <- v


// ConstantMod<'a> represents a constant mod-cell
// and implements IMod<'a> (making use of the core
// class ConstantObject). Note that ConstantMod<'a> allows
// computations to be delayed (which is useful if the
// creation of the value is computationally expensive)
// Note that constant cells are considered equal whenever
// their content is equal. Therefore equality checks will 
// force the evaluation of a constant cell.
type ConstantMod<'a> =
    class
        inherit ConstantObject
        val mutable private value : Lazy<'a>

        member x.Value =
            x.value.Value

        member x.GetValue(caller : IAdaptiveObject) = 
            x.value.Value
            
        interface IMod with
            member x.IsConstant = true
            member x.GetValue(caller) = x.GetValue(caller) :> obj

        interface IMod<'a> with
            member x.GetValue(caller) = x.GetValue(caller)

            
        override x.GetHashCode() =
            let v = x.GetValue(null) :> obj
            if v = null then 0
            else v.GetHashCode()

        override x.Equals o =
            match o with
                | :? IMod<'a> as o when o.IsConstant ->
                    System.Object.Equals(x.GetValue(null), o.GetValue(null))
                | _ -> false

        override x.ToString() =
            x.GetValue(null).ToString()

        new(value : 'a) = ConstantMod<'a>(lazy value)
        new(compute : unit -> 'a) = ConstantMod<'a>( lazy (compute()) )
        new(l : Lazy<'a>) = { value = l }

    end


/// DefaultingModRef<'a> represents a mod ref with an adaptive
/// default value. The resulting IMod corresponds to the default
/// value unless a custom value is set to the ref.
/// By calling Reset (), the initial behaviour can be restored,
/// i.e. the resulting value is dependent on the adaptive default
/// value.
type DefaultingModRef<'a>(computed : IMod<'a>) as this =
    inherit AdaptiveObject()

    let mutable cache = Unchecked.defaultof<'a>
    let mutable isComputed = true
    let mutable tracker = ChangeTracker.trackVersion<'a>


    member x.GetValue(caller) =
        x.EvaluateAlways caller (fun () ->
            if x.OutOfDate && isComputed then
                let v = computed.GetValue(x)
                cache <- v
                v
            else
                cache
        )

    member x.Reset() =
        if not isComputed then
            tracker <- ChangeTracker.trackVersion<'a>
            isComputed <- true

    member x.Value 
        with get() = 
            if isComputed then x.GetValue(null)
            else cache
        and set v =
            let changed =
                if isComputed then 
                    computed.RemoveOutput x
                    isComputed <- false
                    x.Level <- 0
                    true
                else 
                    tracker v || not <| Object.Equals(v, cache)
            if changed then
                cache <- v
                x.MarkOutdated()

    interface IMod with
        member x.IsConstant = false
        member x.GetValue(caller) = x.GetValue(caller) :> obj
            
    interface IMod<'a> with
        member x.GetValue(caller) = x.GetValue(caller)

    interface IModRef<'a> with
        member x.Value 
            with get () = x.Value
            and set v = x.Value <- v
    

/// <summary>
/// defines functions for composing mods and
/// managing evaluation order, etc.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Mod =

    // the attribute system needs to know how to "unpack"
    // modifiable cells for inherited attributes.
    // we therefore need to register a function doing that
    [<OnAardvarkInit>]
    let initialize() =
        Report.BeginTimed "initializing mod system"

        Aardvark.Base.AgHelpers.unpack <- fun o ->
            match o with
                | :? IMod as o -> o.GetValue(null)
                | _ -> o


        Report.End() |> ignore

    open System.Reflection


    [<AbstractClass>]
    type AbstractMod<'a> =
        class
            inherit AdaptiveObject
            val mutable public cache : 'a
            val mutable public scope : Ag.Scope

            abstract member Compute : unit -> 'a
            

            member x.GetValue(caller) =
                x.EvaluateAlways caller (fun () ->
                    if x.OutOfDate then
                        Ag.useScope x.scope (fun () ->
                            x.cache <- x.Compute()
                        )
                    x.cache
                )

            interface IMod with
                member x.IsConstant = false
                member x.GetValue(caller) = x.GetValue(caller) :> obj

            interface IMod<'a> with
                member x.GetValue(caller) = x.GetValue(caller)

            new() =
                { cache = Unchecked.defaultof<'a>; scope = Ag.getContext() }
        end

    // LazyMod<'a> (as the name suggests) implements IMod<'a>
    // and will be evaluated lazily (if not forced to be eager
    // by a callback or subsequent eager computations)
    type LazyMod<'a> =
        class
            inherit AdaptiveObject
            val mutable public cache : 'a
            val mutable public compute : IMod<'a> -> 'a
            val mutable public scope : Ag.Scope


            member x.GetValue(caller) =
                x.EvaluateAlways caller (fun () ->
                    if x.OutOfDate then
                        Ag.useScope x.scope (fun () ->
                            x.cache <- x.compute (x :> _)
                        )
                    x.cache
                )

            interface IMod with
                member x.IsConstant = false
                member x.GetValue(caller) = x.GetValue(caller) :> obj

            interface IMod<'a> with
                member x.GetValue(caller) = x.GetValue(caller)

            new(compute) =
                { cache = Unchecked.defaultof<'a>; compute = compute; scope = Ag.getContext() }
        end

    type internal LazyModWithChangedInputs<'a> =
        class
            inherit AdaptiveObject
            val mutable public cache : 'a
            val mutable public compute : IMod<'a> -> PersistentHashSet<IAdaptiveObject> -> 'a
            val mutable public scope : Ag.Scope
            val mutable changedInputs : PersistentHashSet<IAdaptiveObject>

            override x.InputChanged i =
                System.Threading.Interlocked.Change(&x.changedInputs, PersistentHashSet.add i) |> ignore

            member x.GetValue(caller : IAdaptiveObject) =
                x.EvaluateAlways caller (fun () ->
                    if x.OutOfDate then
                        let changed = System.Threading.Interlocked.Exchange(&x.changedInputs, PersistentHashSet.empty)
                        
                        try
                            Ag.useScope x.scope (fun () ->
                                x.cache <- x.compute (x :> _) changed
                            )
                        with LevelChangedException(t,_,_) as ex ->
                            if t.Id = x.Id then
                                System.Threading.Interlocked.Change(&x.changedInputs, PersistentHashSet.union changed) |> ignore
                            raise ex

                    x.cache
                )

            interface IMod with
                member x.IsConstant = false
                member x.GetValue(caller) = x.GetValue(caller) :> obj

            interface IMod<'a> with
                member x.GetValue(caller) = x.GetValue(caller)

            new(compute) =
                { cache = Unchecked.defaultof<'a>; compute = compute; scope = Ag.getContext(); changedInputs = PersistentHashSet.empty }
        end


    // EagerMod<'a> re-uses LazyMod<'a> and extends it with
    // a Mark function evaluating the cell whenever it is marked
    // as outOfDate. Since eager mods might want to "cancel"
    // the change propagation process when equal values are
    // observed EagerMod can also be created with a custom 
    // equality function.
    type internal EagerMod<'a>(input : IMod<'a>, eq : Option<'a -> 'a -> bool>) as this=
        inherit LazyMod<'a>(fun s -> input.GetValue(s))

        let hasChanged = ChangeTracker.trackCustom<'a> eq

        member x.Input = input

        override x.Inputs = Seq.singleton (input :> IAdaptiveObject)

        override x.Mark() =
            let newValue = x.GetValue(null)
            x.OutOfDate <- false

            if hasChanged newValue then
                x.cache <- newValue
                true
            else
                false

        new(compute) = EagerMod(compute, None)

    // LaterMod<'a> is a special construct for forcing lazy
    // evaluation for a specific cell. Note that this needs to
    // be "transparent" (knowning its input) since we need to
    // be able to undo the effect.
    type internal LaterMod<'a>(input : IMod<'a>) as this=
        inherit LazyMod<'a>(fun s -> input.GetValue(s))

        override x.Inputs = Seq.singleton (input :> IAdaptiveObject)


        member x.Input = input

    // TimeMod represents a changeable cell which will always
    // be outdated as soon as control-flow leaves the mod system.
    // Its value will always be the time when pulled.
    // NOTE that this cannot be implemented using the other mod-types
    //      since caching (of the current time) is not desired here.
    type internal TimeMod private() =
        inherit AdaptiveObject()

        static let instance = TimeMod() :> IMod<DateTime>
        static member Instance = instance

        member x.GetValue(caller : IAdaptiveObject) =
            x.EvaluateAlways caller (fun () ->
                AdaptiveObject.Time.Outputs.Add x |> ignore
                DateTime.Now
            )

        interface IMod with
            member x.IsConstant = false
            member x.GetValue(caller) = x.GetValue(caller) :> obj

        interface IMod<DateTime> with
            member x.GetValue(caller) = x.GetValue(caller)
            
    type internal DecoratorMod<'b>(m : IMod, f : obj -> 'b) =
        inherit AdaptiveDecorator(m)

        member x.GetValue(caller : IAdaptiveObject) =
            m.GetValue(caller) |> f

        interface IMod with
            member x.IsConstant = m.IsConstant
            member x.GetValue(caller) = x.GetValue(caller) :> obj

        interface IMod<'b> with
            member x.GetValue(caller) = x.GetValue(caller)

    type internal DecoratorMod<'a, 'b>(m : IMod<'a>, f : 'a -> 'b) =
        inherit AdaptiveDecorator(m)

        member x.GetValue(caller : IAdaptiveObject) =
            m.GetValue(caller) |> f

        interface IMod with
            member x.IsConstant = m.IsConstant
            member x.GetValue(caller) = x.GetValue(caller) :> obj

        interface IMod<'b> with
            member x.GetValue(caller) = x.GetValue(caller)


    type internal MapMod<'a, 'b>(inner : IMod<'a>, f : 'a -> 'b) =
        inherit AbstractMod<'b>()

        override x.Inputs = 
            Seq.singleton (inner :> IAdaptiveObject)

        override x.Compute() =
            inner.GetValue x |> f

    type internal Map2Mod<'a, 'b, 'c>(a : IMod<'a>, b : IMod<'b>, f : 'a -> 'b -> 'c) =
        inherit AbstractMod<'c>()

        override x.Inputs = 
            Seq.ofList [a :> IAdaptiveObject; b :> IAdaptiveObject]
        
        override x.Compute() =
            f (a.GetValue x) (b.GetValue x)

    type internal MapNMod<'a, 'b>(a : seq<IMod<'a>>, f : list<'a> -> 'b) =
        inherit AbstractMod<'b>()
        let a = Seq.toList a

        let mutable dirty : PersistentHashSet<IMod<'a>> = PersistentHashSet.empty
        let store = Dictionary<IMod<'a>, 'a>()

        override x.InputChanged i =
            match i with
                | :? IMod<'a> as i -> System.Threading.Interlocked.Change(&dirty, PersistentHashSet.add i) |> ignore
                | _ -> ()

        override x.Inputs = 
            a |> Seq.cast
        
        override x.Compute() =
            let dirty = System.Threading.Interlocked.Exchange(&dirty, PersistentHashSet.empty)

            for d in PersistentHashSet.toSeq dirty do
                store.[d] <- d.GetValue x

            a |> List.map (fun v -> store.[v]) |> f


    type internal BindMod<'a, 'b>(m : IMod<'a>, f : 'a -> IMod<'b>) =
        inherit AbstractMod<'b>()

        let mutable inner : Option<'a * IMod<'b>> = None
        let mutable changedInputs = PersistentHashSet.empty

        override x.Inputs =
            seq {
                yield m :> IAdaptiveObject
                match inner with
                    | Some (_,i) -> yield i :> IAdaptiveObject
                    | None -> ()
            }

        override x.InputChanged i =
            System.Threading.Interlocked.Change(&changedInputs, PersistentHashSet.add i) |> ignore

        override x.Compute() =
            let changed = System.Threading.Interlocked.Exchange(&changedInputs, PersistentHashSet.empty)

            // whenever the result is outOfDate we
            // need to pull the input's value
            // Note that the input is not necessarily outOfDate at this point
            let v = m.GetValue x
            //let cv = hasChanged v

            let mChanged = PersistentHashSet.contains (m :> IAdaptiveObject) changed

            match inner with
                // if the function argument has not changed
                // since the last execution we expect f to return
                // the identical cell
                | Some (v', inner) when not mChanged ->
                    // since the inner cell might be outOfDate we
                    // simply pull its value and don't touch any in-/outputs.
                    inner.GetValue x
                        
                | _ ->
                    // whenever the argument's value changed we need to 
                    // re-execute the function and store the new inner cell.
                    let i = f v :> IMod<_>
                    let old = inner
                    inner <- Some (v, i)


                    match old with
                        // if there was an old inner cell which
                        // is different from the new one we
                        // remove the resulting cell from the old
                        // outputs and add it to the new ones. 
                        | Some (_,old) when old <> i -> 
                            old.RemoveOutput x |> ignore

                        // in any other case the graph remained
                        // constant and we don't change a thing.
                        | _ -> ()

                    // finally we pull the value from the
                    // new inner cell.
                    i.GetValue x

    type internal Bind2Mod<'a, 'b, 'c>(ma : IMod<'a>, mb : IMod<'b>, f : 'a -> 'b -> IMod<'c>) =
        inherit AbstractMod<'c>()

        let mutable inner : Option<'a * 'b * IMod<'c>> = None
        let mutable changedInputs = PersistentHashSet.empty

        override x.Inputs =
            seq {
                yield ma :> IAdaptiveObject
                yield mb :> IAdaptiveObject
                match inner with
                    | Some (_,_,i) -> yield i :> IAdaptiveObject
                    | None -> ()
            }

        override x.InputChanged i =
            System.Threading.Interlocked.Change(&changedInputs, PersistentHashSet.add i) |> ignore

        override x.Compute() =
            let changed = System.Threading.Interlocked.Exchange(&changedInputs, PersistentHashSet.empty)
            let a = ma.GetValue x
            let b = mb.GetValue x

            let ca = PersistentHashSet.contains (ma :> IAdaptiveObject) changed
            let cb = PersistentHashSet.contains (mb :> IAdaptiveObject) changed

            match inner with
                | Some (va, vb, inner) when not ca && not cb ->
                    inner.GetValue x
                | _ ->

                    let i = f a b :> IMod<_>
                    let old = inner
                    inner <- Some (a, b, i)

                    match old with
                        | Some (_,_,old) when old <> i -> 
                            old.RemoveOutput x |> ignore

                        | _ -> ()

                        
                    i.GetValue x 
 
    type internal DynamicMod<'a>(f : unit -> IMod<'a>) =
        inherit AbstractMod<'a>()

        let inner = lazy (f())

        override x.Inputs =
            if inner.IsValueCreated then Seq.singleton (inner.Value :> IAdaptiveObject)
            else Seq.empty

        override x.Compute() =
            inner.Value.GetValue x




    let private scoped (f : 'a -> 'b) =
        let scope = Ag.getContext()
        fun v -> Ag.useScope scope (fun () -> f v)


    let private callbackTable = ConditionalWeakTable<IMod, ConcurrentHashSet<IDisposable>>()
    /// <summary>
    /// creates a custom modifiable cell using the given
    /// compute function. If no inputs are added to the
    /// cell it will actually be constant.
    /// However the system will not statically assume the
    /// cell to be constant in any case.
    /// </summary>
    let custom (compute : IMod<'a> -> 'a) : IMod<'a> =
        LazyMod(compute) :> IMod<_>

    
    /// <summary>
    /// registers a callback for execution whenever the
    /// cells value might have changed and returns a disposable
    /// subscription in order to unregister the callback.
    /// Note that the callback will be executed immediately
    /// once here.
    /// Note that this function does not hold on to the created disposable, i.e.
    /// if the disposable as well as the source dies, the callback dies as well.
    /// If you use callbacks to propagate changed to other mods by using side-effects
    /// (which you should not do), use registerCallbackKeepDisposable in order to
    /// create a gc to the fresh disposable.
    /// registerCallbackKeepDisposable only destroys the callback, iff the associated
    /// disposable is disposed.
    /// </summary>
    let unsafeRegisterCallbackNoGcRoot (f : 'a -> unit) (m : IMod<'a>) =
        let result =
            m.AddEvaluationCallback(fun () ->
                m.GetValue(null) |> f
            )

        let set = callbackTable.GetOrCreateValue(m)
        set.Add result |> ignore
        result

    [<Obsolete("use unsafeRegisterCallbackNoGcRoot or unsafeRegisterCallbackKeepDisposable instead")>]
    let registerCallback f m = unsafeRegisterCallbackNoGcRoot f m

    let private undyingCallbacks = ConcurrentHashSet<IDisposable>()

    /// <summary>
    /// registers a callback for execution whenever the
    /// set's value might have changed and returns a disposable
    /// subscription in order to unregister the callback.
    /// Note that the callback will be executed immediately
    /// once here.
    /// In contrast to registerCallbackNoGcRoot, this function holds on to the
    /// fresh disposable, i.e. even if the input set goes out of scope,
    /// the disposable still forces the complete computation to exist.
    /// When disposing the assosciated disposable, the gc root disappears and
    /// the computation can be collected.
    /// </summary>
    let unsafeRegisterCallbackKeepDisposable f m = 
        let d = unsafeRegisterCallbackNoGcRoot f m
        undyingCallbacks.Add d |> ignore
        { new IDisposable with
            member x.Dispose() =
                d.Dispose()
                undyingCallbacks.Remove d |> ignore
        }

    /// <summary>
    /// changes the value of the given cell. Note that this
    /// function may only be used inside a current transaction.
    /// </summary>
    let change (m : IModRef<'a>) (value : 'a) =
        m.Value <- value


    /// <summary>
    /// initializes a new constant cell using the given value.
    /// </summary>
    let constant (v : 'a)  =
        ConstantMod<'a>(v) :> IMod<_>

    /// <summary>
    /// initializes a new modifiable input cell using the given value.
    /// </summary>
    let init (v : 'a) =
        ModRef v

    /// see DefaultingModRef
    let initDefault (initial : IMod<'a>) =
        DefaultingModRef initial

    /// <summary>
    /// initializes a new constant cell using the given lazy value.
    /// </summary>
    let delay (f : unit -> 'a) =
        ConstantMod<'a> (scoped f) :> IMod<_>

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// resulting in a new dependent cell.
    /// </summary>
    let map (f : 'a -> 'b) (m : IMod<'a>) =
        if m.IsConstant then
            let f = scoped f
            delay (fun () -> m.GetValue(null) |> f)
        else
            MapMod(m, f) :> IMod<_>

    /// <summary>
    /// adaptively applies a function to two cell's values
    /// resulting in a new dependent cell.
    /// </summary>
    let map2 (f : 'a -> 'b -> 'c) (m1 : IMod<'a>) (m2 : IMod<'b>)=
        match m1.IsConstant, m2.IsConstant with
            | (true, true) -> 
                delay (fun () -> f (m1.GetValue(null)) (m2.GetValue(null))) 
            | (true, false) -> 
                map (fun b -> f (m1.GetValue(null)) b) m2
            | (false, true) -> 
                map (fun a -> f a (m2.GetValue(null))) m1
            | (false, false) ->
                Map2Mod(m1, m2, f) :> IMod<_>

    /// <summary>
    /// creates a custom modifiable cell using the given
    /// compute function and adds all given inputs to the
    /// resulting cell.
    /// </summary>
    let mapCustom (f : IMod<'a> -> 'a) (inputs : list<IAdaptiveObject>) =
        let r = custom f
        for i in inputs do
            i.AddOutput r
        r

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// without creating a new cell (maintaining equality, id, etc.)
    /// NOTE: this combinator assumes that the given function 
    ///       is really cheap (e.g. field-access, cast, etc.)
    /// </summary>
    let mapFast (f : 'a -> 'b) (m : IMod<'a>) =
        DecoratorMod<'a, 'b>(m, f) :> IMod<_>

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// without creating a new cell (maintaining equality, id, etc.)
    /// NOTE: this combinator assumes that the given function 
    ///       is really cheap (e.g. field-access, cast, etc.)
    /// </summary>
    let mapFastObj (f : obj -> 'b) (m : IMod) =
        DecoratorMod<'b>(m, f) :> IMod<_>

    /// <summary>
    /// adaptively casts a value to the desired type
    /// and fails if the cast is invalid.
    /// NOTE that this does not create a new cell but instead
    ///      "decorates" the given cell.
    /// </summary>
    let inline cast (m : IMod) : IMod<'b> =
        mapFastObj unbox m

    

    /// <summary>
    /// creates a modifiable cell using the given inputs
    /// and compute function (being evaluated whenever any of
    /// the inputs changes.
    /// </summary>
    let mapN (f : seq<'a> -> 'b) (inputs : seq<#IMod<'a>>) =
        MapNMod(Seq.toList (Seq.cast inputs), List.toSeq >> f) :> IMod<_>
//        let objs = inputs |> Seq.cast |> Seq.toList
//        objs |> mapCustom (fun s ->
//            let values = inputs |> Seq.map (fun m -> m.GetValue s) |> Seq.toList
//            f values
//        )

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// and returns a new dependent cell holding the inner
    /// cell's content.
    /// </summary>
    let bind (f : 'a -> #IMod<'b>) (m : IMod<'a>) =
        if m.IsConstant then
            m.GetValue(null) |> f :> IMod<_>
        else
            BindMod(m, fun v -> f v :> _) :> IMod<_>
      

    /// <summary>
    /// adaptively applies a function to two cell's values
    /// and returns a new dependent cell holding the inner
    /// cell's content.
    /// </summary>
    let bind2 (f : 'a -> 'b -> #IMod<'c>) (ma : IMod<'a>) (mb : IMod<'b>) =
        match ma.IsConstant, mb.IsConstant with
            | (true, true) ->
                f (ma.GetValue(null)) (mb.GetValue(null)) :> IMod<_>
            | (false, true) ->
                bind (fun a -> (f a (mb.GetValue(null))) :> IMod<_>) ma
            | (true, false) ->
                bind (fun b -> (f (ma.GetValue(null)) b) :> IMod<_>) mb
            | (false, false) ->
                Bind2Mod(ma, mb, fun a b -> (f a b) :> _) :> IMod<_>

    /// <summary>
    /// creates a dynamic cell using the given function
    /// while maintaining lazy evaluation.
    /// </summary>
    let dynamic (f : unit -> IMod<'a>) =
        DynamicMod(f) :> IMod<_>


    /// <summary>
    /// forces the evaluation of a cell and returns its current value
    /// </summary>
    let force (m : IMod<'a>) =
        m.GetValue(null)

    /// <summary>
    /// creates a new cell forcing the evaluation of the
    /// given one during change propagation (making it eager)
    /// </summary>
    let rec onPush (m : IMod<'a>) =
        if m.IsConstant then 
            m.GetValue(null) |> ignore
            m
        else
            match m with
                | :? LaterMod<'a> as m -> onPush m.Input
                | :? EagerMod<'a> -> m
                | _ ->
                    let res = EagerMod(m)
                    res.GetValue(null) |> ignore
                    res :> IMod<_>


    /// <summary>
    /// creates a new cell forcing the evaluation of the
    /// given one during change propagation (making it eager)
    /// using a 
    /// </summary>
    let rec onPushCustomEq (eq : 'a -> 'a -> bool) (m : IMod<'a>) =
        if m.IsConstant then 
            m.GetValue(null) |> ignore
            m
        else
            match m with
                | :? LaterMod<'a> as m -> onPush m.Input
                | :? EagerMod<'a> -> m
                | _ ->
                    let res = EagerMod(m, Some eq)
                    res.GetValue(null) |> ignore
                    res :> IMod<_>

    /// <summary>
    /// creates a new cell forcing the evaluation of the
    /// given one to be lazy (on demand)
    /// NOTE: onPull does not maintain equality
    ///       for constant-cells
    /// </summary>
    let rec onPull (m : IMod<'a>) =
        if m.IsConstant then
            LaterMod(m) :> IMod<_>
        else
            match m with
                | :? EagerMod<'a> as m ->
                    onPull m.Input
                | _ -> m


    /// <summary>
    /// creates a new cell by starting the given async computation.
    /// until the computation is completed the cell will contain None.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary>
    let asyncTask (task : System.Threading.Tasks.Task<'a>) : IMod<Option<'a>> =
        if task.IsCompleted then
            constant (Some task.Result)
        else
            let r = init None
            let a = task.GetAwaiter()
            a.OnCompleted(fun () -> 
                transact (fun () -> 
                    let v = a.GetResult()
                    change r (Some v)
                )
            )
            r :> IMod<_>

    /// <summary>
    /// creates a new cell by starting the given async computation.
    /// until the computation is completed the cell will contain None.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary>
    let async (a : Async<'a>) : IMod<Option<'a>> =
        let task = a |> Async.StartAsTask
        asyncTask task

    /// <summary>
    /// creates a new cell by starting the given async computation.
    /// until the computation is completed the cell will contain the default value.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary> 
    let asyncWithDefault (defaultValue : 'a) (a : Async<'a>) : IMod<'a> =
        let task = a |> Async.StartAsTask
        if task.IsCompleted then
            constant task.Result
        else
            let r = init defaultValue
            let a = task.GetAwaiter()
            a.OnCompleted(fun () -> 
                transact (fun () -> 
                    let v = a.GetResult()
                    change r v
                )
            )
            r :> IMod<_>           

    /// <summary>
    /// creates a new cell starting the given async computation when a value is requested for the first time.
    /// until the computation is completed the cell will contain None.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary>
    let lazyAsync (run : Async<'a>) : IMod<Option<'a>> =
        let task : ref<Option<System.Threading.Tasks.Task<'a>>> = ref None

        let res = ref Unchecked.defaultof<_>
        res :=
            custom (fun s ->
                match !task with
                    | Some t ->
                        if t.IsCompleted then 
                            let res = t.Result
                            Some res
                        else 
                            None
                    | None ->
                        let t = Async.StartAsTask run
                        task := Some t
                        t.GetAwaiter().OnCompleted(fun () -> transact (fun () -> res.Value.MarkOutdated()))

                        None
            )

        !res

    /// <summary>
    /// creates a new cell starting the given async computation when a value is requested for the first time.
    /// until the computation is completed the cell will contain the default value.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary> 
    let lazyAsyncWithDefault (defaultValue : 'a) (run : Async<'a>) : IMod<'a> =
        let task : ref<Option<System.Threading.Tasks.Task<'a>>> = ref None

        let res = ref Unchecked.defaultof<_>
        res :=
            custom (fun s ->
                match !task with
                    | Some t ->
                        if t.IsCompleted then 
                            let res = t.Result
                            res
                        else 
                            defaultValue
                    | None ->
                        let t = Async.StartAsTask run
                        task := Some t
                        t.GetAwaiter().OnCompleted(fun () -> transact (fun () -> res.Value.MarkOutdated()))

                        defaultValue
            )

        !res

    /// <summary>
    /// creates a new cell by starting the given async computation.
    /// upon evaluation of the cell it will wait until the async computation has finished
    /// </summary>
    let start (run : Async<'a>) =
        let task = run |> Async.StartAsTask
        if task.IsCompleted then
            constant task.Result
        else
            custom (fun s ->
                task.Result
            )

    /// <summary>
    /// a changeable cell which will always be outdated when control-flow leaves
    /// the mod-system. Its value will always hold the current time (DateTime.Now) 
    /// </summary>
    let time = TimeMod.Instance



    [<Obsolete("Use Mod.init instead")>]
    let inline initMod (v : 'a) = init v

    [<Obsolete("Use Mod.constant instead")>]
    let inline initConstant (v : 'a) = constant v

    [<Obsolete("Use Mod.onPush instead")>]
    let inline always (m : IMod<'a>) = onPush m

    [<Obsolete("Use Mod.onPull instead")>]
    let inline later (m : IMod<'a>) = onPull m



[<AutoOpen>]
module ModExtensions =
    
    // reflect the type argument used by a given
    // mod-type or return None if no mod type.
    let rec private extractModTypeArg (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IMod<_>> then
            Some (t.GetGenericArguments().[0])
        else
            let iface = t.GetInterface(typedefof<IMod<_>>.FullName)
            if iface = null then None
            else extractModTypeArg iface

    /// <summary>
    /// matches all types implementing IMod<'a> and
    /// extracts typeof<'a> using reflection.
    /// </summary>
    let (|ModOf|_|) (t : Type) =
        match extractModTypeArg t with
            | Some t -> ModOf t |> Some
            | None -> None


