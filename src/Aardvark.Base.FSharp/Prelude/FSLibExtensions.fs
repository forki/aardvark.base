﻿namespace Aardvark.Base

open System

[<AutoOpen>]
module Prelude =

    let inc (a:byref<int>) = a <- a + 1
    let dec (a:byref<int>) = a <- a - 1
    
    let inline isNull (a : 'a) =
        match a with
            | null -> true
            | _ -> false

    module Map =
        let union (l : Map<'k, 'v>) (r : Map<'k, 'v>) =
            let mutable result = l
            for KeyValue(k,v) in r do
                result <- Map.add k v result
            result

    module Seq =
        let iter' (f : 'a -> 'b) (s : seq<'a>) = 
            for i in s do
                f i |> ignore 

        let repeat (n : int) (f : unit -> unit) =
            for i in 1..n do
                f()

        let repeat' (n : int) (f : unit -> 'a) =
            for i in 1..n do
                f() |> ignore

        let partition (f : 'a -> bool) (xs : seq<'a>) = 
            let xs = xs |> Seq.map (fun a -> f a, a) |> Seq.cache

            (xs |> Seq.filter (fun (r,v) -> not r) |> Seq.map snd, xs |> Seq.filter (fun (r,v) -> r) |> Seq.map snd)

        let chooseOption (f : 'a -> Option<'b>) (xs : seq<Option<'a>>) : seq<Option<'b>> =
            seq {
                for x in xs do
                    match x with
                     | None -> ()
                     | Some x -> yield f x
            }

        let forany (f : 'a -> bool) (s : seq<'a>) =
            let e = s.GetEnumerator()
            let mutable r = false
            while not r && e.MoveNext() do
                if f e.Current then
                    r <- true
            e.Dispose()
            r

    module List = 

        let rec forany (f : 'a -> bool) (l : list<'a>) =
            match l with
                | [] -> false
                | x::xs ->
                    if f x then
                        true
                    else
                        forany f xs

        //Experimental Results show that this implementation is faster than all other ones maintaining the list's order
        let rec private partitionAcc (f : 'a -> bool) (source : list<'a>) (l : System.Collections.Generic.List<'a>) (r : System.Collections.Generic.List<'a>) =
             match source with
                | [] ->
                    let mutable ll = []
                    let mutable rr = []

                    for i in 1..l.Count do
                        let i = l.Count - i
                        ll <- l.[i]::ll

                    for i in 1..r.Count do
                        let i = r.Count - i
                        rr <- r.[i]::rr

                    (ll,rr)

                | x::xs ->
                    if f x then
                        l.Add(x)
                        partitionAcc f xs l r
                    else
                        r.Add(x)
                        partitionAcc f xs l r   

        let partition (f : 'a -> bool) (source : list<'a>) = 
            partitionAcc f source (System.Collections.Generic.List()) (System.Collections.Generic.List())

    module Array =
        let rec private forany' (f : 'a -> bool) (index : int) (a : 'a[]) =
            if index >= a.Length then false
            else
                if f a.[index] then true
                else forany' f (index + 1) a

        let forany (f : 'a -> bool) (a : 'a[]) =
            forany' f 0 a
    
    module Disposable =

        let inline dispose v = (^a : (member Dispose : unit -> unit) v)

    module Option =
        
        let inline defaultValue (fallback : 'a) (option : Option<'a>) = 
            match option with
             | Some value ->  value
             | None -> fallback

    (* Error datastructure *)
    type Error<'a> = Success of 'a
                   | Error of string

    (* Either left or right *)
    type Either<'a,'b> = Left of 'a 
                       | Right of 'b


    let toFunc (f : 'a -> 'b) : Func<'a, 'b> =
        Func<'a, 'b>(f)

    let fromFunc (f : Func<'a, 'b>) : 'a -> 'b =
        fun x -> f.Invoke(x)

    let dowhile (f : unit -> bool) =
        while f() do ()

    //VERY IMPORTANT CODE
    let uncurry (f : 'a -> 'b -> 'c) = fun (a,b) -> f a b
    let curry (f : 'a * 'b -> 'c) = fun a b -> f (a,b)

    let schönfinkel = curry
    let deschönfinkel = uncurry

    let frege = curry
    let unfrege = uncurry

    let ಠ_ಠ str = failwith str

    let inline flip f a b = f b a

    let private BinaryDynamicImpl mn : ('T -> 'U -> 'V) =
        let aty = typeof<'T>
        let bty = typeof<'U>
        let minfo = aty.GetMethod(mn,[| aty;bty |])
        (fun x y -> unbox<_>(minfo.Invoke(null,[| box x; box y|])))

    type private PowDynamicImplTable<'T>() = 
        static let result : ('T -> 'T -> 'T) = 
            let aty = typeof<'T>
            if   aty.Equals(typeof<sbyte>)      then unbox<_>(fun (x:sbyte) (y:sbyte)           -> Fun.Pow(int x, float y) |> sbyte)
            elif aty.Equals(typeof<int16>)      then unbox<_>(fun (x:int16) (y:int16)           -> Fun.Pow(int x, float y) |> int16)
            elif aty.Equals(typeof<int32>)      then unbox<_>(fun (x:int32) (y:int32)           -> Fun.Pow(x, float y) |> int)
            elif aty.Equals(typeof<int64>)      then unbox<_>(fun (x:int64) (y:int64)           -> Fun.Pow(x, float y) |> int64)

            elif aty.Equals(typeof<byte>)        then unbox<_>(fun (x:byte) (y:byte)             -> Fun.Pow(int x, float y) |> byte)
            elif aty.Equals(typeof<uint16>)      then unbox<_>(fun (x:uint16) (y:uint16)         -> Fun.Pow(int x, float y) |> uint16)
            elif aty.Equals(typeof<uint32>)      then unbox<_>(fun (x:uint32) (y:uint32)         -> Fun.Pow(int x, float y) |> uint32)
            elif aty.Equals(typeof<uint64>)      then unbox<_>(fun (x:uint64) (y:uint64)         -> Fun.Pow(int64 x, float y) |> uint64)

            elif aty.Equals(typeof<nativeint>)  then unbox<_>(fun (x:nativeint) (y:nativeint)   -> Fun.Pow(int64 x, float y) |> nativeint)
            elif aty.Equals(typeof<float>)      then unbox<_>(fun (x:float) (y:float)           -> Fun.Pow(x, y) )
            elif aty.Equals(typeof<float32>)    then unbox<_>(fun (x:float32) (y:float32)       -> Fun.Pow(x, y))
            elif aty.Equals(typeof<decimal>)    then unbox<_>(fun (x:decimal) (y:decimal)       -> Fun.Pow(float x, float y) |> decimal)
            else BinaryDynamicImpl "Pow" 
        static member Result : ('T -> 'T -> 'T) = result

    let PowDynamic (x : 'T) (y : 'T) = PowDynamicImplTable<'T>.Result x y

    let inline pow< ^T when ^T : (static member (*) : ^T -> ^T -> ^T)> (x: ^T) (y : ^T) : ^T = 
        PowDynamic x y

    let inline clamp (min : ^a) (max : ^a) (x : ^a) =
        if x < min then min
        elif x > max then max
        else x


    let (|Windows|Linux|Mac|) (p : System.OperatingSystem) =
        match p.Platform with
            | System.PlatformID.Unix -> Linux
            | System.PlatformID.MacOSX -> Mac
            | _ -> Windows


[<AutoOpen>]
module Threading =
    open System.Threading

    type Interlocked with
        static member Change(location : byref<'a>, f : 'a -> 'a) =
            let mutable v = location
            let mutable res = f v
            let mutable r = Interlocked.CompareExchange(&location, res, v)

            while not (System.Object.ReferenceEquals(v,r)) do
                v <- r
                res <- f v
                r <- Interlocked.CompareExchange(&location, res, v)

            res


module GenericValues =
    open System.Reflection

    let inline zero< ^a when ^a : (static member (-) : ^a -> ^a -> ^a)> : ^a =
        let t = typeof< ^a>
        let pi = t.GetProperty("Zero", BindingFlags.Static ||| BindingFlags.Public)
        let fi = t.GetField("Zero", BindingFlags.Static ||| BindingFlags.Public)

        if pi <> null then pi.GetValue(null) |> unbox
        elif fi <> null then fi.GetValue(null) |> unbox
        else Unchecked.defaultof< ^a>

module Caching =

    let memoTable = System.Collections.Concurrent.ConcurrentDictionary<obj,obj>()
    let cacheFunction (f : 'a -> 'b) (a : 'a) :  'b =
        memoTable.GetOrAdd((a,f) :> obj, fun (o:obj) -> f a :> obj)  |> unbox<'b>
            

module IO =
    open System.IO

    let alterFileName str f = Path.Combine (Path.GetDirectoryName str, f (Path.GetFileName str))

    let openStreamStrong path = 
        if File.Exists path 
        then File.Delete path
        new FileStream(path, FileMode.CreateNew)
