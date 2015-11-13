namespace Aardvark.Base.Incremental.Scratch

open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework
open FsUnit

open Aardvark.Base.Incremental
open Aardvark.Base

type CRange1 = CRange1 of cset<int * int>

type IAReader<'a> =
    inherit IDisposable
    inherit IAdaptiveObject

    abstract member Range : Range1i
    abstract member Content : unit -> array<'a>
    abstract member GetDelta : IAdaptiveObject * Range1i -> list<Range1i> 

type AbstractReader<'a>(interestedIn : Range1i) =
    inherit AdaptiveObject()

    abstract member GetDelta : IAdaptiveObject * Range1i -> list<Range1i> 

    member x.Dispose() = ()

    interface IAReader<'a> with
        member x.Dispose() = x.Dispose()
        member x.Range = interestedIn
        member x.Content () = failwith ""
        member x.GetDelta(caller,forRange) = x.GetDelta(caller,forRange)
            

module AArrayReaders =

    type MapReader<'a,'b>(input : IAReader<'a>, f : 'a -> 'b, operatesOn : Range1i) =
        inherit AbstractReader<'b>(operatesOn)

        let store = Array.zeroCreate operatesOn.Size

        override x.ComputeDelta() =
            let dirtyRegions = input.GetDelta(x,operatesOn)
            for r in dirtyRegions do
                for i in r.Min .. r.Max do
                    store.[i] <- f source.[i]
                    

        override x.Inputs = Seq.singleton (input :> IAdaptiveObject)

    let map (operatesOn : Range1i) f (input : IAReader<'a>) =
        new MapReader<_,_>(input,f, operatesOn) :> IAReader<'a>

type aarray<'a> =
    abstract member GetReader : Option<Range1i> -> IAReader<'a> 
    abstract member Range : Range1i


module CAArray =

    type EmitReader<'a>(input : array<'a>, filterRegion : Range1i) =
        inherit AbstractReader<'a>(filterRegion)

        member x.Emit (dirtyRanges : seq<Range1i>) =
             if not x.OutOfDate then
                match getCurrentTransaction() with
                    | Some t ->
                        t.Enqueue x
                    | _ ->
                        failwith "[EmitReader] cannot emit without transaction"

    type caarray<'a>(xs : array<'a>) =

        let readers = List<EmitReader<_>>()
        let ranges = List<Range1i>()

        member x.WriteRange(range : Range1i, f) =
            f xs
            ranges.Add range
            for r in readers do r.Emit (ranges :> seq<_>)
            ranges.Clear()

        interface aarray<'a> with
            member x.GetReader s = 
                let r = 
                    match s with
                     | None -> new EmitReader<_>(xs, Range1i(0,xs.Length-1)) 
                     | Some r -> new EmitReader<_>(xs, r) 
                readers.Add r
                r :> _
            member x.Range = Range1i(0,xs.Length-1)
        

    let ofArray (xs : array<'a>) = caarray xs

module AArray =

    open AArrayReaders

    let map f (xs : aarray<_>) : aarray<_> =
        { new aarray<_> with
            member x.GetReader s = xs.GetReader s |> map xs.Range f
            member x.Range = xs.Range
        }

    let test() =
        let xs = CAArray.ofArray [| 1 .. 1000 |]
        xs.WriteRange(Range1i(0,10), 
            fun x -> 
                for i in 0 .. 10 do x.[i] <- i * 10
        )
        ()
        

module CRange =

    let init (a,b) = CRange1 (CSet.ofList [a,b])

    let invalidate (a,b) (CRange1 c) =
        let xs = CSet.toSeq c |> Seq.toList
        let rec contained xs  = 
            match xs with
             | (ia,ib)::xs -> if a >= ia && b <= ib then true else contained xs
             | [] -> false
        if contained xs then ()
        else transact (fun () -> c.Add (a,b) |> ignore)