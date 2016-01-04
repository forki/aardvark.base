namespace Aardvark.Base

open System

[<AllowNullLiteral>]
type IAwaitable = interface end
[<AllowNullLiteral>]
type IAwaitable<'a> =
    inherit IAwaitable  

[<AllowNullLiteral>]
type Awaitable<'a>() = 
    interface IAwaitable<'a>

[<AllowNullLiteral>]
type Awaitable() = 
    interface IAwaitable

type IEvent =
    abstract member Next : IAwaitable with get
    abstract member Values : IObservable<System.Reactive.Unit> with get

type IEvent<'a> =
    inherit IEvent
    abstract member Latest : 'a with get
    abstract member Next : IAwaitable<'a> with get
    abstract member Values : IObservable<'a> with get

type EventSourceSlim<'a>(blub : bool) =
    interface IEvent<'a> with
        member x.Next with get () = Unchecked.defaultof<IAwaitable<'a>>
        member x.Values with get () =Unchecked.defaultof<IObservable<'a>>
        member x.Next with get () = Unchecked.defaultof<IAwaitable>
        member x.Values with get () =Unchecked.defaultof<IObservable<System.Reactive.Unit>>
        member x.Latest with get () = failwith ""
    member x.Emit v = ()
    new() = EventSourceSlim(false)

type EventSource<'a>() =
    interface IEvent<'a> with
        member x.Next with get () = Unchecked.defaultof<IAwaitable<'a>>
        member x.Values with get () = Unchecked.defaultof<IObservable<'a>>
        member x.Next with get () = Unchecked.defaultof<IAwaitable>
        member x.Values with get () =Unchecked.defaultof<IObservable<System.Reactive.Unit>>
        member x.Latest with get () = failwith ""
    member x.Emit v = ()
    new(v) = EventSource()