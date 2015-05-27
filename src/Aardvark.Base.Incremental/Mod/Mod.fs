﻿namespace Aardvark.Base.Incremental

open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Collections.Concurrent
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
    abstract member GetValue : unit -> obj

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
    abstract member GetValue : unit -> 'a

/// <summary>
/// ModRef<'a> represents a changeable input
/// cell which can be changed by the user and
/// implements IMod<'a>
/// </summary>
type ModRef<'a>(value : 'a) =
    inherit AdaptiveObject()
    let mutable value = value

    member x.Value
        with get() = value
        and set v =
            if not <| Object.Equals(v, value) then
                value <- v
                x.MarkOutdated()

    member x.GetValue() =
        x.EvaluateAlways (fun () ->
            value
        )

    interface IMod with
        member x.IsConstant = false
        member x.GetValue() = x.GetValue() :> obj

    interface IMod<'a> with
        member x.GetValue() = x.GetValue()

/// <summary>
/// defines functions for composing mods and
/// managing evaluation order, etc.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Mod =
    open Aardvark.Base

    // the attribute system needs to know how to "unpack"
    // modifiable cells for inherited attributes.
    // we therefore need to register a function doing that
    [<OnAardvarkInit>]
    let initialize() =
        Report.BeginTimed "initializing mod system"

        Aardvark.Base.AgHelpers.unpack <- fun o ->
            match o with
                | :? IMod as o -> o.GetValue()
                | _ -> o


        Report.End() |> ignore

    // LazyMod<'a> (as the name suggests) implements IMod<'a>
    // and will be evaluated lazily (if not forced to be eager
    // by a callback or subsequent eager computations)
    type LazyMod<'a> =
        class
            inherit AdaptiveObject
            val mutable public cache : 'a
            val mutable public compute : unit -> 'a
            val mutable public scope : Ag.Scope

            member x.GetValue() =
                x.EvaluateIfNeeded x.cache (fun () ->
                    Ag.useScope x.scope (fun () ->
                        x.cache <- x.compute()
                    )
                    x.cache
                )


            interface IMod with
                member x.IsConstant = false
                member x.GetValue() = x.GetValue() :> obj

            interface IMod<'a> with
                member x.GetValue() = x.GetValue()

            new(compute) =
                { cache = Unchecked.defaultof<'a>; compute = compute; scope = Ag.getContext() }
        end

    // EagerMod<'a> re-uses LazyMod<'a> and extends it with
    // a Mark function evaluating the cell whenever it is marked
    // as outOfDate. Since eager mods might want to "cancel"
    // the change propagation process when equal values are
    // observed EagerMod can also be created with a custom 
    // equality function.
    type private EagerMod<'a>(compute : unit -> 'a, eq : Option<'a -> 'a -> bool>) =
        inherit LazyMod<'a>(compute)

        let eq = defaultArg eq (fun a b -> System.Object.Equals(a,b))

        override x.Mark() =
            let newValue = compute()
            x.OutOfDate <- false

            if eq newValue x.cache then
                false
            else
                x.cache <- newValue
                true

        new(compute) = EagerMod(compute, None)

    // ConstantMod<'a> represents a constant mod-cell
    // and implements IMod<'a> (making use of the core
    // class ConstantMod). Note that ConstantMod allows
    // computations to be delayed (which is useful if the
    // creation of the value is computationally expensive)
    // Note that constant cells are considered equal whenever
    // their content is equal.
    type private ConstantMod<'a> =
        class
            inherit ConstantObject
            val mutable public compute : unit -> 'a
            val mutable public value : Option<'a>

            member x.GetValue() = 
                match x.value with
                    | Some v -> v
                    | None -> 
                        let v = x.compute()
                        x.value <- Some v
                        v
            
            interface IMod with
                member x.IsConstant = true
                member x.GetValue() = x.GetValue() :> obj

            interface IMod<'a> with
                member x.GetValue() = x.GetValue()

            
            override x.GetHashCode() =
                let v = x.GetValue() :> obj
                if v = null then 0
                else v.GetHashCode()

            override x.Equals o =
                match o with
                    | :? IMod<'a> as o when o.IsConstant ->
                        System.Object.Equals(x.GetValue(), o.GetValue())
                    | _ -> false

            override x.ToString() =
                x.GetValue().ToString()

            private new(value : Option<'a>, compute : unit -> 'a) = { value = value; compute = compute }

            static member Value(value : 'a) = ConstantMod(Some value, unbox) :> IMod<_>
            static member Lazy(compute : unit -> 'a) = ConstantMod(None, compute) :> IMod<_>
        end

    let private scoped (f : 'a -> 'b) =
        let scope = Ag.getContext()
        fun v -> Ag.useScope scope (fun () -> f v)

    let private scoped2 (f : 'a -> 'b -> 'c) =
        let scope = Ag.getContext()
        fun a b -> Ag.useScope scope (fun () -> f a b)


    /// <summary>
    /// creates a custom modifiable cell using the given
    /// compute function. If no inputs are added to the
    /// cell it will actually be constant.
    /// However the system will not statically assume the
    /// cell to be constant in any case.
    /// </summary>
    let custom (compute : unit -> 'a) : IMod<'a> =
        LazyMod(scoped compute) :> IMod<_>


    let private callbackTable = ConditionalWeakTable<IMod, ConcurrentHashSet<IDisposable>>()
    type private CallbackSubscription<'a>(m : IMod, cb : unit -> unit, live : ref<bool>, last : ref<Option<'a>>, set : ConcurrentHashSet<IDisposable>) =
        
        member x.Dispose() = 
            if !live then
                live := false
                m.MarkingCallbacks.Remove cb |> ignore
                set.Remove x |> ignore
                last := None

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        override x.Finalize() =
            try x.Dispose()
            with _ -> ()

    /// <summary>
    /// registers a callback for execution whenever the
    /// cells value might have changed and returns a disposable
    /// subscription in order to unregister the callback.
    /// Note that the callback will be executed immediately
    /// once here.
    /// </summary>
    let registerCallback (f : 'a -> unit) (m : IMod<'a>) =
        let f = scoped f
        let self = ref id
        let live = ref true
        let last = ref None

        self := fun () ->
            if !live then
                try
                    let value = m.GetValue()
                    match !last with
                        | Some v when Object.Equals(v, value) -> ()
                        | _ -> 
                            f value
                            last := Some value
                finally 
                    m.MarkingCallbacks.Add !self |> ignore
        
        lock m (fun () ->
            !self ()
        )

        let set = callbackTable.GetOrCreateValue(m)

        let s = new CallbackSubscription<'a>(m, !self, live, last, set)
        set.Add s |> ignore
        s :> IDisposable


    /// <summary>
    /// changes the value of the given cell. Note that this
    /// function may only be used inside a current transaction.
    /// </summary>
    let change (m : ModRef<'a>) (value : 'a) =
        m.Value <- value

    /// <summary>
    /// initializes a new modifiable input cell using the given value.
    /// </summary>
    let initMod (v : 'a) =
        ModRef v

    /// <summary>
    /// initializes a new constant cell using the given value.
    /// </summary>
    let initConstant (v : 'a) =
        ConstantMod.Value v

    /// <summary>
    /// initializes a new constant cell using the given lazy value.
    /// </summary>
    let delay (f : unit -> 'a) =
        ConstantMod.Lazy (scoped f)

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// resulting in a new dependent cell.
    /// </summary>
    let map (f : 'a -> 'b) (m : IMod<'a>) =
        let f = scoped f
        if m.IsConstant then
            ConstantMod.Lazy (fun () -> m.GetValue() |> f)
        else
            let res = LazyMod(fun () -> m.GetValue() |> f)
            m.AddOutput res
            res :> IMod<_>

    /// <summary>
    /// adaptively applies a function to two cell's values
    /// resulting in a new dependent cell.
    /// </summary>
    let map2 (f : 'a -> 'b -> 'c) (m1 : IMod<'a>) (m2 : IMod<'b>)=
        match m1.IsConstant, m2.IsConstant with
            | (true, true) -> 
                delay (fun () -> f (m1.GetValue()) (m2.GetValue()))
            | (true, false) -> 
                map (fun b -> f (m1.GetValue()) b) m2
            | (false, true) -> 
                map (fun a -> f a (m2.GetValue())) m1
            | (false, false) ->
                let f = scoped2 f
                let res = LazyMod(fun () -> f (m1.GetValue()) (m2.GetValue()))
                m1.AddOutput res
                m2.AddOutput res
                res :> IMod<_>

    /// <summary>
    /// creates a custom modifiable cell using the given
    /// compute function and adds all given inputs to the
    /// resulting cell.
    /// </summary>
    let mapCustom (f : unit -> 'a) (inputs : list<IAdaptiveObject>) =
        let r = custom f
        for i in inputs do
            i.AddOutput r
        r

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// and returns a new dependent cell holding the inner
    /// cell's content.
    /// </summary>
    let bind (f : 'a -> #IMod<'b>) (m : IMod<'a>) =
        if m.IsConstant then
            m.GetValue() |> f :> IMod<_>
        else
            let f = scoped f
            let inner : ref<Option<'a * IMod<'b>>> = ref None
            
            // just a reference-cell for allowing self-recursive
            // access in the compute function below.
            let res = ref <| Unchecked.defaultof<LazyMod<'b>>
            res := 
                LazyMod(fun () -> 
                    // whenever the result is outOfDate we
                    // need to pull the input's value
                    // Note that the input is not necessarily outOfDate at this point
                    let v = m.GetValue()

                    match !inner with
                        // if the function argument has not changed
                        // since the last execution we expect f to return
                        // the identical cell
                        | Some (v', inner) when Object.Equals(v, v') ->
                            // since the inner cell might be outOfDate we
                            // simply pull its value and don't touch any in-/outputs.
                            inner.GetValue()
                        
                        | _ ->
                            // whenever the argument's value changed we need to 
                            // re-execute the function and store the new inner cell.
                            let i = f v :> IMod<_>
                            let old = !inner
                            inner := Some (v, i)


                            match old with
                                // if there was no old inner cell
                                // we simply add ourselves as output
                                // of the new inner value
                                | None -> 
                                    i.AddOutput !res |> ignore
                                
                                // if there was an old inner cell which
                                // is different from the new one we
                                // remove the resulting cell from the old
                                // outputs and add it to the new ones. 
                                | Some (_,old) when old <> i -> 
                                    old.RemoveOutput !res |> ignore
                                    i.AddOutput !res |> ignore

                                // in any other case the graph remained
                                // constant and we don't change a thing.
                                | _ -> ()

                            // finally we pull the value from the
                            // new inner cell.
                            i.GetValue()
                        

                )

            // since m is statically known to be an input
            // of the resulting cell we add the edge to the 
            // dependency graph.
            m.AddOutput !res |> ignore
            !res :> IMod<_>

    /// <summary>
    /// adaptively applies a function to two cell's values
    /// and returns a new dependent cell holding the inner
    /// cell's content.
    /// </summary>
    let bind2 (f : 'a -> 'b -> #IMod<'c>) (ma : IMod<'a>) (mb : IMod<'b>) =
        
        match ma.IsConstant, mb.IsConstant with
            | (true, true) ->
                f (ma.GetValue()) (mb.GetValue()) :> IMod<_>
            | (false, true) ->
                bind (fun a -> (f a (mb.GetValue())) :> IMod<_>) ma
            | (true, false) ->
                bind (fun b -> (f (ma.GetValue()) b) :> IMod<_>) mb
            | (false, false) ->
                let f = scoped2 f
                let inner : ref<Option<'a * 'b * IMod<'c>>> = ref None

                // for a detailed description see bind above
                let res = ref <| Unchecked.defaultof<LazyMod<'c>>
                res := 
                    LazyMod(fun () -> 
                        let a = ma.GetValue()
                        let b = mb.GetValue()

                        match !inner with
                            | Some (va, vb, inner) when Object.Equals(a, va) && Object.Equals(b, vb) ->
                                inner.GetValue()
                            | _ ->
                                let i = f a b :> IMod<_>
                                let old = !inner
                                inner := Some (a, b, i)

                                match old with
                                    | None -> 
                                        i.AddOutput !res |> ignore

                                    | Some (_,_,old) when old <> i -> 
                                        old.RemoveOutput !res |> ignore
                                        i.AddOutput !res |> ignore

                                    | _ -> ()

                        
                                i.GetValue()
                        

                    )
                ma.AddOutput !res |> ignore
                mb.AddOutput !res |> ignore
                !res :> IMod<_>

    /// <summary>
    /// forces the evaluation of a cell and returns its current value
    /// </summary>
    let force (m : IMod<'a>) =
        m.GetValue()

    /// <summary>
    /// creates a new cell forcing the evaluation of the
    /// given one during change propagation (making it eager)
    /// </summary>
    let always (m : IMod<'a>) =
        if m.IsConstant then 
            m
        else
            match m with
                | :? EagerMod<'a> -> m
                | _ ->
                    let res = EagerMod(fun () -> m.GetValue())
                    m.AddOutput(res)
                    res.GetValue() |> ignore
                    res :> IMod<_>

    /// <summary>
    /// creates a new cell by starting the given async computation.
    /// until the computation is completed the cell will contain None.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary>
    let async (a : Async<'a>) : IMod<Option<'a>> =
        let task = a |> Async.StartAsTask
        if task.IsCompleted then
            initConstant (Some task.Result)
        else
            let r = initMod None
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
    /// until the computation is completed the cell will contain the default value.
    /// as soon as the computation is finished it will contain the resulting value.
    /// </summary> 
    let asyncWithDefault (defaultValue : 'a) (a : Async<'a>) : IMod<'a> =
        let task = a |> Async.StartAsTask
        if task.IsCompleted then
            initConstant task.Result
        else
            let r = initMod defaultValue
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
            custom (fun () ->
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
            custom (fun () ->
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

