﻿namespace Aardvark.Base.Incremental

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Collections
open System.Collections.Concurrent
open Aardvark.Base
open System.Collections.Generic

[<CompiledName("IAdaptiveMap")>]
type amap<'k, 'v when 'k : equality> =
    abstract member ASet : aset<'k * 'v>
    //abstract member GetReader : unit -> IMapReader<'k, 'v>

[<CompiledName("ChangeableMap")>]
type cmap<'k, 'v when 'k : equality>(initial : seq<'k * 'v>) =
    let cset = cset initial
    let set = cset :> aset<_>
    let content = Dict.ofSeq initial

    member x.Count = lock content (fun () -> content.Count)

    member x.ContainsKey key = lock content (fun () -> content.ContainsKey key)

    member x.Add(k,v) =
        lock content (fun () -> 
            content.Add(k,v)
            CSet.add (k,v) cset |> ignore
        )

    member x.Remove(k) =
        lock content (fun () -> 
            match content.TryRemove k with
                | (true, v) ->
                    CSet.remove (k,v) cset
                | _ ->
                    false
        )

    member x.TryRemove(k : 'k, [<Out>] res : byref<'v>) =
        let w,r = 
            lock content (fun () -> 
                match content.TryRemove k with
                    | (true, v) ->
                        CSet.remove (k,v) cset, v
                    | _ ->
                        false, Unchecked.defaultof<_>
            )
        res <- r
        w

    member x.Item
        with get k = lock content (fun () -> content.[k])
        and set k v =
            lock content (fun () -> 
                match content.TryGetValue k with
                    | (true, old) ->
                        CSet.applyDeltas [Add (k,v); Rem (k,old)] cset
                    | _ ->
                        CSet.add (k,v) cset |> ignore

                content.[k] <- v
            )

    member x.Clear() =
        lock content (fun () -> 
            content.Clear()
            CSet.clear cset
        )

    member x.Keys = lock content (fun () -> content.Keys)
    member x.Values = lock content (fun () -> content.Values)

    member x.TryGetValue(k : 'k, [<Out>] v : byref<'v>) =
        let (w,value) =
            lock content (fun () -> 
                content.TryGetValue(k)
            )
        v <- value
        w


    interface IEnumerable with
        member x.GetEnumerator() = content.GetEnumerator() :> IEnumerator

    interface IEnumerable<KeyValuePair<'k, 'v>> with
        member x.GetEnumerator() = content.GetEnumerator() :> IEnumerator<_>

    interface IDict<'k, 'v> with
        member x.Add(k,v) = x.Add(k,v)
        member x.Remove(k) = x.Remove(k)
        member x.Item
            with get k = x.[k]
            and set k v = x.[k] <- v

        member x.TryGetValue(k : 'k, v : byref<'v>) =
            content.TryGetValue(k, &v)

        member x.ContainsKey(k) = content.ContainsKey k
        member x.Keys = content.Keys
        member x.Values = content.Values
        member x.KeyValuePairs = content.KeyValuePairs

    interface amap<'k, 'v> with
        member x.ASet = set 

    new() = cmap Seq.empty

module private AMapUtils =
    
    let add (content : VersionedDictionary<_,_>) (key : 'k) (v : 'v) =
        let set = 
            match content.TryGetValue key with
                | (true, set) -> set
                | _ ->
                    let set = ReferenceCountingSet<'v>() :> IVersionedSet<_>
                    content.[key] <- set
                    set
        set.Add v
           
    let remove (content : VersionedDictionary<_,IVersionedSet<_>>) (key : 'k) (v : 'v) =
        match content.TryGetValue key with
            | (true, set) ->
                if set.Remove v then
                    if set.Count = 0 then content.Remove key |> ignore
                    true
                else
                    false
            | _ ->
                false         

    let apply (content : VersionedDictionary<_,_>) (deltas : list<Delta<'k * 'v>>) =
        [
            for d in deltas do
                match d with
                    | Add (k,v) ->
                        if add content k v then yield d
                    | Rem (k,v) ->
                        if remove content k v then yield d
        ]

module CMap =
    let empty<'k, 'v when 'k : equality> : cmap<'k, 'v> = cmap []

    let ofSeq (s : seq<'k * 'v>) = cmap<'k, 'v> s
    let ofList (s : list<'k * 'v>) = cmap<'k, 'v> s
    let ofArray (s : array<'k * 'v>) = cmap<'k, 'v> s

    let add (key : 'k) (value : 'v) (map : cmap<'k, 'v>) = map.Add(key, value)

    let get (key : 'k) (map : cmap<'k, 'v>) = map.[key]
    let set (key : 'k) (value : 'v) (map : cmap<'k, 'v>) = map.[key] <- value

    let remove (key : 'k) (map : cmap<'k, 'v>) = map.Remove key

    let tryRemove (key : 'k) (map : cmap<'k, 'v>) =
        match map.TryRemove key with
            | (true, v) -> Some v
            | _ -> None

    let clear (map : cmap<'k, 'v>) = map.Clear()

    let containsKey (key : 'k) (map : cmap<'k, 'v>) = map.ContainsKey key

    let count (map : cmap<'k, 'v>) = map.Count

    let toSeq (map : cmap<'k, 'v>) = map |> Seq.map (fun (KeyValue(k,v)) -> k,v)

    let toList (map : cmap<'k, 'v>) = map |> toSeq |> Seq.toList

    let toArray (map : cmap<'k, 'v>) = map |> toSeq |> Seq.toArray

    let unionWith (other : IDictionary<'k, 'v>) (map : cmap<'k, 'v>) =
        for (KeyValue(k,v)) in other do
            map.[k] <- v

module AMap =

    type private SetMap<'k, 'v when 'k : equality>(aset : aset<'k * 'v>) =
        interface amap<'k, 'v> with
            member x.ASet = aset
            //member x.GetReader() = new SetMapReader<'k, 'v>(aset.GetReader()) :> IMapReader<_,_>

    type private LookupReader<'k, 'v  when 'k : equality>(input : IReader<'k * 'v>, key : 'k) as this =
        inherit ASetReaders.AbstractReader<'v>()
        do input.AddOutput this

        override x.ComputeDelta() = 
            input.GetDelta() 
                |> List.choose (fun d -> 
                    match d with
                        | Add (k,v) when k = key -> Some (Add v)
                        | Rem (k,v) when k = key -> Some (Rem v)
                        | _ -> None
                    )

        override x.Release() =
            input.RemoveOutput x
            input.Dispose()


    let private wrap (f : 'k -> 'v -> 'a) =
        fun (k,v) -> (k, f k v)

    let private setmap (set : aset<'k * 'v>) =
        SetMap(set) :> amap<_,_>

    type private EmptyMapImpl<'k, 'v when 'k : equality>() =
        static let empty : amap<'k, 'v> = ASet.empty |> setmap
        static member Instance = empty

    let empty<'k, 'v when 'k : equality> = EmptyMapImpl<'k, 'v>.Instance

    let single (k : 'k) (v : 'v) = (k,v) |> ASet.single |> setmap

    let ofASet (s : aset<'k * 'v>) = s |> setmap

    let toASet (m : amap<'k, 'v>) = m.ASet

    let ofSeq (s : seq<'k * 'v>) = s |> ASet.ofSeq |> setmap
        
    let ofList (s : list<'k * 'v>) = s |> ASet.ofList |> setmap
    
    let ofArray (s : array<'k * 'v>) = s |> ASet.ofArray |> setmap
    
    let ofMod (m : IMod<'k * 'v>) =
        m |> ASet.ofMod |> setmap

    let toMod (m : amap<'k, 'v>) =
        let r = m.ASet.GetReader()
        let content = VersionedDictionary<'k,IVersionedSet<'v>>()
        let res = Mod.custom(fun () ->
            r.GetDelta() |> AMapUtils.apply content |> ignore
            content :> IVersionedDictionary<_,_>
        )
        r.AddOutput res
        res

    let map (f : 'k -> 'v -> 'a) (m : amap<'k, 'v>) =
        m.ASet |> ASet.map (wrap f) |> setmap

    let bind (f : 'a -> amap<'k,'v>) (m : IMod<'a>) =
        m |> ASet.bind (f >> toASet) |> setmap

    let bind2 (f : 'a -> 'b -> amap<'k, 'v>) (ma : IMod<'a>) (mb : IMod<'b>) =
        ASet.bind2 (fun a b -> f a b |> toASet) ma mb |> setmap

    let collect (f : 'k -> 'v -> amap<'k1, 'v1>) (m : amap<'k, 'v>) =
        m.ASet |> ASet.collect (fun (k,v) -> (f k v).ASet) |> setmap

    let collect' (f : 'a -> amap<'k, 'v>) (set : aset<'a>) =
        set |> ASet.collect (f >> toASet) |> setmap

    let choose (f : 'k -> 'v -> Option<'a>) (m : amap<'k, 'v>) =
        m.ASet |> ASet.choose (fun (k,v) -> match f k v with | Some a -> Some (k,a) | None -> None) |> setmap

    let filter (f : 'k -> 'v -> bool) (m : amap<'k, 'v>) =
        m.ASet |> ASet.filter (uncurry f) |> setmap

    let union (maps : aset<amap<'k, 'v>>) =
        maps |> ASet.collect toASet |> setmap

    let union' (maps : seq<amap<'k, 'v>>) =
        maps |> Seq.map toASet |> ASet.union' |> setmap

    let mapM (f : 'k -> 'v -> IMod<'n>) (m : amap<'k, 'v>) =
        m |> toASet |> ASet.mapM ( fun (k, v) -> f k v |> Mod.map (fun v -> (k,v)) ) |> setmap

    let filterM (f : 'k -> 'v -> IMod<bool>) (m : amap<'k, 'v>) =
        m |> toASet |> ASet.filterM (uncurry f) |> setmap

    let chooseM (f : 'k  -> 'v -> IMod<Option<'n>>) (m : amap<'k, 'v>) =
        m |> toASet |> ASet.chooseM ( fun (k, v) -> f k v |> Mod.map (fun v -> match v with | Some v -> Some(k,v) | None -> None) ) |> setmap

    let tryFindAll (key : 'k) (m : amap<'k, 'v>) =
        ASet.AdaptiveSet(fun () -> new LookupReader<'k, 'v>(m.ASet.GetReader(), key) :> IReader<'v>) :> aset<_>

    let tryFind (key : 'k) (m : amap<'k, 'v>) =
        tryFindAll key m 
            |> ASet.toMod 
            |> Mod.map (fun set -> 
                if set.Count = 0 then 
                    None 
                else 
                    Some (set |> Seq.head)
                )

    [<Obsolete("use unsafeRegisterCallbackNoGcRoot or unsafeRegisterCallbackKeepDisposable instead")>]
    let registerCallback (f : list<Delta<'k * 'v>> -> unit) (map : amap<'k,'v>) =
        map.ASet |> ASet.unsafeRegisterCallbackNoGcRoot f

    let unsafeRegisterCallbackKeepDisposable (f : list<Delta<'k * 'v>> -> unit) (map : amap<'k,'v>) =
        map.ASet |> ASet.unsafeRegisterCallbackKeepDisposable f

    let unsafeRegisterCallbackNoGcRoot (f : list<Delta<'k * 'v>> -> unit) (map : amap<'k,'v>) =
        map.ASet |> ASet.unsafeRegisterCallbackNoGcRoot f

[<AutoOpen>]
module ``ASet grouping`` =
    module ASet =
        let groupBy (f : 'v -> 'k) (xs : aset<'v>) : amap<'k,'v> = 
            xs |> ASet.map (fun v -> (f v, v)) |> AMap.ofASet