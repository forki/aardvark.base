﻿namespace Aardvark.Base.Incremental

/// provides special operators for composing IMods
module Operators =
    
    /// adaptively adds two values
    let inline (%+) (l : IMod<'a>) (r : IMod<'b>) = Mod.map2 (+) l r

    /// adaptively subtracts two values
    let inline (%-) (l : IMod<'a>) (r : IMod<'b>) = Mod.map2 (-) l r

    /// adaptively mutiplies two values
    let inline (%*) (l : IMod<'a>) (r : IMod<'b>) = Mod.map2 (*) l r

    /// adaptively divides two values
    let inline (%/) (l : IMod<'a>) (r : IMod<'b>) = Mod.map2 (/) l r

    /// creates an adaptive cell providing the value of "l && r"
    let inline (%&&) (l : IMod<bool>) (r : IMod<bool>) = Mod.map2 (&&) l r

    /// creates an adaptive cell providing the value of "l || r"
    let inline (%||) (l : IMod<bool>) (r : IMod<bool>) = Mod.map2 (||) l r

    /// expresses an adaptive "if then else" expression (e.g. m %? a %. b <=> if m then a else b)
    let (%?) (l : IMod<bool>) (vt : 'a) : 'a -> IMod<'a> = fun vf -> l |> Mod.map (fun v -> if v then vt else vf)

    /// expresses an adaptive "if then else" expression (e.g. m %? a %. b <=> if m then a else b)
    let (%.) (l : 'a -> IMod<'a>) (r : 'a) =  l r

    /// forces the value of a cell
    let inline (!!) (m : IMod<'a>) = Mod.force m

    /// creates a constant cell containing the given value
    let inline (~~) (v : 'a) = Mod.constant v

    /// creates a changeable cell containing the given value
    let inline mref (v : 'a) = Mod.init v

[<AutoOpen>]
module ``Observable extensions`` =
    open System
    open System.Reactive
    open System.Reactive.Linq
    
    type ObservableBuilder() =
        member x.For(o : IObservable<'a>, f : 'a -> IObservable<'b>) =
            o.SelectMany f

        member x.Yield(v) = 
            Observable.Return(v)

        member x.Delay(f : unit -> IObservable<'a>) = f

        member x.For(m : IMod<'a>, f : 'a -> IObservable<'b>) =
            let mo = m |> Mod.toObservable
            mo.SelectMany f

        member x.Combine(l : IObservable<'a>, r : unit -> IObservable<'a>) =
            l.Concat (Observable.Defer(r))


        member x.Zero() = 
            Observable.Empty()

        member x.While(guard : unit -> #IMod<bool>, body : unit -> IObservable<'b>) : IObservable<'b>  =
            let guard = guard() :> IMod<bool> |> Mod.toObservable

            Observable.Create(fun (obs : IObserver<'b>) ->
                let active = ref false
                let inner = ref { new IDisposable with member x.Dispose() = () }
 
                let rec run() =
                    body().Subscribe(fun v ->
                        obs.OnNext(v)
                        let old = !inner
                        inner := run()
                        old.Dispose()

                    )

                let guardSub =
                    guard.Subscribe (fun v ->
                        if v then
                            if not !active then
                                inner := Observable.Defer(body).Repeat().Subscribe obs

                            active := true
                        else
                            if !active then
                                inner.Value.Dispose()
                                obs.OnCompleted()
                                ()

                            active := false
                    )
                { new IDisposable with
                    member x.Dispose() =
                        guardSub.Dispose()
                        inner.Value.Dispose()
                }
            )

        member x.While(guard : unit -> bool, body : unit -> IObservable<'a>) =
            if guard() then
                body().Concat(Observable.Defer(fun () -> x.While(guard, body)))
            else
                Observable.Empty()
            
        member x.Run(f : unit -> IObservable<'a>) = 
            f()


    module Observable =
        let private noDisposable = { new IDisposable with member x.Dispose() = () }

        let latest (o : IObservable<'a>) : IMod<Option<'a>> =
            let r = Mod.init None
            let subscription = ref noDisposable
            subscription :=
                o.Subscribe (
                    { new IObserver<'a> with
                        member x.OnNext(v) = 
                            r.UnsafeCache <- Some v
                            let mark = Locking.read r (fun () -> not r.OutOfDate)
                            if mark then
                                transact(fun () -> r.MarkOutdated())
                        member x.OnCompleted() =
                            subscription.Value.Dispose()
                            subscription := noDisposable
                        member x.OnError e =
                            subscription.Value.Dispose()
                            subscription := noDisposable
                    }      
                )

            r :> IMod<_>

    let obs = ObservableBuilder()


[<AutoOpen>]
module ``Computation Expression Builders`` =
    
    type AdaptiveBuilder() =
        let constantUnit = Mod.constant ()

        member x.Bind(tup : IMod<'a> * IMod<'b>, f : 'a * 'b -> IMod<'c>) : IMod<'c> =
            Mod.bind2 (fun a b -> f(a,b)) (fst tup) (snd tup)

        member x.Bind(m : IMod<'a>, f : 'a -> IMod<'b>) =
            Mod.bind f m

        member x.Return (v : 'a) =
            Mod.constant v :> IMod<_>

        member x.ReturnFrom(m : IMod<'a>) = 
            m

        member x.Zero() = constantUnit

        member x.Combine(l : IMod<unit>, r : IMod<'a>) =
            Mod.map2 (fun () r -> r) l r

        member x.Delay (f : unit -> IMod<'a>) =
            constantUnit |> Mod.bind f

        

    type ASetBuilder() =
        member x.Yield (v : 'a) =
            ASet.single v

        member x.YieldFrom (set : aset<'a>) =
            set

        member x.Bind(m : IMod<'a> * IMod<'b>, f : 'a * 'b -> aset<'c>) =
            ASet.bind2 (fun a b -> f(a,b)) (fst m) (snd m)

        member x.Bind(m : IMod<'a>, f : 'a -> aset<'b>) =
            ASet.bind f m

        member x.Bind(m : System.Threading.Tasks.Task<'a>, f : 'a -> aset<'b>) =
            ASet.bindTask f m

        member x.Bind(m : Async<'a>, f : 'a -> aset<'b>) =
            ASet.bindTask f (Async.StartAsTask m)

        member x.For(s : aset<'a>, f : 'a -> aset<'b>) =
            ASet.collect f s

        member x.For(s : seq<'a>, f : 'a -> aset<'b>) =
            ASet.collect' f s

        member x.Zero() =
            ASet.empty

        member x.Delay(f : unit -> aset<'a>) =
            f()

        member x.Combine(l : aset<'a>, r : aset<'a>) =
            ASet.union' [l;r]

    type AListBuilder() =
        member x.Yield (v : 'a) =
            AList.single v

        member x.YieldFrom (set : alist<'a>) =
            set

        member x.Bind(m : IMod<'a> * IMod<'b>, f : 'a * 'b -> alist<'c>) =
            AList.bind2 (fun a b -> f(a,b)) (fst m) (snd m)

        member x.Bind(m : IMod<'a>, f : 'a -> alist<'b>) =
            AList.bind f m

        member x.For(s : alist<'a>, f : 'a -> alist<'b>) =
            AList.collect f s

        member x.For(s : seq<'a>, f : 'a -> alist<'b>) =
            AList.collect' f s

        member x.Zero() =
            AList.empty

        member x.Delay(f : unit -> alist<'a>) =
            f()

        member x.Combine(l : alist<'a>, r : alist<'a>) =
            AList.concat' [l;r]


    module Mod =
        let toASet (m : IMod<'a>) =
            ASet.ofMod m

        let toAList (m : IMod<'a>) =
            AList.ofMod m


    let adaptive = AdaptiveBuilder()
    let aset = ASetBuilder()
    let alist = AListBuilder()

open System.Runtime.CompilerServices

[<AbstractClass; Sealed; Extension>]
type EvaluationExtensions() =

    [<Extension>]
    static member inline GetValue(x : IMod) = x.GetValue(null)


    [<Extension>]
    static member inline GetValue(x : IMod<'a>) = x.GetValue(null)

    [<Extension>]
    static member inline GetDelta(x : IReader<'a>) = x.GetDelta(null)
    [<Extension>]
    static member inline Update(x : IReader<'a>) = x.Update(null)

    [<Extension>]
    static member inline GetDelta(x : IListReader<'a>) = x.GetDelta(null)
    [<Extension>]
    static member inline Update(x : IListReader<'a>) = x.Update(null)

    [<Extension>]
    static member inline Evaluate(x : afun<'a, 'b>, v : 'a) = x.Evaluate(null, v)
