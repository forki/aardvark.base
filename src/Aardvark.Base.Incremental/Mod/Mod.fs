﻿namespace Aardvark.Base.Incremental

open System

[<AllowNullLiteral>]
type IMod =
    inherit IAdaptiveObject
    abstract member IsConstant : bool
    abstract member GetValue : unit -> obj

[<AllowNullLiteral>]
type IMod<'a> =
    inherit IMod
    abstract member GetValue : unit -> 'a

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
        x.EvaluateIfNeeded x.Value (fun () ->
            x.Value
        )

    interface IMod with
        member x.IsConstant = false
        member x.GetValue() = x.GetValue() :> obj

    interface IMod<'a> with
        member x.GetValue() = x.GetValue()

module Mod =
    open Aardvark.Base

    [<OnAardvarkInit>]
    let initialize() =
        Report.BeginTimed "initializing mod system"

        Aardvark.Base.AgHelpers.unpack <- fun o ->
            match o with
                | :? IMod as o -> o.GetValue()
                | _ -> o


        Report.End() |> ignore

    type private LazyMod<'a> =
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
                v.GetHashCode()

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

    let custom (compute : unit -> 'a) : IMod<'a> =
        LazyMod(compute) :> IMod<_>

    let registerCallback (f : 'a -> unit) (m : IMod<'a>) =
        let self = ref id
        let live = ref true
        self := fun () ->
            if !live then
                try
                    m.GetValue() |> f
                finally 
                    m.MarkingCallbacks.Add !self |> ignore
        
        lock m (fun () ->
            !self ()
        )

        { new IDisposable with
            member x.Dispose() = live := false; m.MarkingCallbacks.Remove !self |> ignore
        }

    let change (m : ModRef<'a>) (value : 'a) =
        m.Value <- value

    let initMod (v : 'a) =
        ModRef v

    let initConstant (v : 'a) =
        ConstantMod.Value v

    let delay (f : unit -> 'a) =
        ConstantMod.Lazy f

    let map (f : 'a -> 'b) (m : IMod<'a>) =
        if m.IsConstant then
            ConstantMod.Lazy (fun () -> m.GetValue() |> f)
        else
            let res = LazyMod(fun () -> m.GetValue() |> f)
            m.AddOutput res
            res :> IMod<_>

    let map2 (f : 'a -> 'b -> 'c) (m1 : IMod<'a>) (m2 : IMod<'b>)=
        match m1.IsConstant, m2.IsConstant with
            | (true, true) -> 
                delay (fun () -> f (m1.GetValue()) (m2.GetValue()))
            | (true, false) -> 
                map (fun b -> f (m1.GetValue()) b) m2
            | (false, true) -> 
                map (fun a -> f a (m2.GetValue())) m1
            | (false, false) ->
                let res = LazyMod(fun () -> f (m1.GetValue()) (m2.GetValue()))
                m1.AddOutput res
                m2.AddOutput res
                res :> IMod<_>


    let bind (f : 'a -> #IMod<'b>) (m : IMod<'a>) =
        if m.IsConstant then
            m.GetValue() |> f :> IMod<_>
        else
            let res = ref <| Unchecked.defaultof<LazyMod<'b>>
            let inner : ref<Option<'a * IMod<'b>>> = ref None
            res := 
                LazyMod(fun () -> 
                    let v = m.GetValue()

                    match !inner with
                        | Some (v', inner) when Object.Equals(v, v') ->
                            inner.GetValue()
                        | _ ->
                            let i = f v :> IMod<_>
                            let old = !inner
                            inner := Some (v, i)

                            match old with
                                | None -> i.AddOutput !res |> ignore
                                | Some (_,old) when old <> i -> 
                                    old.RemoveOutput !res |> ignore
                                    i.AddOutput !res |> ignore

                                | _ -> ()

                        
                            i.GetValue()
                        

                )
            m.AddOutput !res |> ignore
            !res :> IMod<_>

    let bind2 (f : 'a -> 'b -> #IMod<'c>) (ma : IMod<'a>) (mb : IMod<'b>) =
        match ma.IsConstant, mb.IsConstant with
            | (true, true) ->
                f (ma.GetValue()) (mb.GetValue()) :> IMod<_>
            | (false, true) ->
                bind (fun a -> (f a (mb.GetValue())) :> IMod<_>) ma
            | (true, false) ->
                bind (fun b -> (f (ma.GetValue()) b) :> IMod<_>) mb
            | (false, false) ->
                let res = ref <| Unchecked.defaultof<LazyMod<'c>>
                let inner : ref<Option<'a * 'b * IMod<'c>>> = ref None
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


    let force (m : IMod<'a>) =
        m.GetValue()

    let outOfDate (m : IMod<'a>) =
        m.OutOfDate

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


[<AutoOpen>]
module ModExtensions =

    let rec private extractModTypeArg (t : Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IMod<_>> then
            Some (t.GetGenericArguments().[0])
        else
            let iface = t.GetInterface(typedefof<IMod<_>>.FullName)
            if iface = null then None
            else extractModTypeArg iface

    let (|ModOf|_|) (t : Type) =
        match extractModTypeArg t with
            | Some t -> ModOf t |> Some
            | None -> None

