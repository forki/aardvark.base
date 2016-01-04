namespace Aardvark.Base

[<AutoOpen;ReflectedDefinition>]
module Base = 
    open System
    open System.Runtime.InteropServices 
    open System.Runtime.CompilerServices
    open System.Collections.Generic
    open System.Linq

    module Fun =
        let Max (a,b)= failwith ""
        let Pow(a,b) = failwith ""
        let NextPowerOfTwo(x : int) = failwith ""
        let Log2 v = failwith ""
        let Ceiling v = failwith ""
    


    type Range1i(min:int,max:int)= 
        member x.Min = min
        member x.Max = max


    [<Extension>]
    type BaseExtensions() = 

        [<Extension>]
        static member Log2Int(this : float) = failwith ""



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Range1i =
        let Invalid : Range1i = failwith ""


    type Symbol = SymbolC of int

    [<AutoOpen>]
    module Blub =
        module Symbol =
            let Empty = SymbolC 0
            let CreateNewGuid () = failwith ""
            let Create str = failwith ""

    type Symbol with 
        member x.ToGuid sym = failwith ""
        member x.Id = failwith ""
        member x.IsNotEmpty = failwith ""
        member x.IsPositive = failwith ""
        member x.IsNegative = failwith ""
        member x.IsEmpty = failwith ""


    type Dict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
    type SymbolDict<'v> = Dict<Symbol,'v>

    [<AutoOpen>]
    module SymDictCtor =
        let SymbolDict<'v> () : SymbolDict<'v> = System.Collections.Generic.Dictionary<Symbol,'v>()


    type C4f = C4f of float32 * float32 * float32 * float32
    type C4b = C4b of byte * byte * byte * byte

    type C3f = C4f of float32 * float32 * float32
    type C3b = C4b of byte * byte * byte

    type V4i = V3i of int * int * int * int
    type V4f = V4f of float32 * float32 * float32 * float32

    type V3i = V3i of int * int * int
    type V3f = V4f of float32 * float32 * float32
    type V3d = V4d of float * float * float

    type V4d = V4d of float * float * float * float

    type V2i = V2i of int * int 
    type V2f = V2f of float32 * float32
    type V2d = V2d of float * float

    type M22f = int
    type M22d = int
    type M33f = int
    type M33d = int
    type M34f = int
    type M44d = int
    type M34d = int
    type M44f = int

    [<AutoOpen>]
    module Props =
        module V2i =
            let Zero = V2i(0,0)

    type V2i with
        member x.X = let (V2i(x,y)) = x in x
        member x.Y = let (V2i(x,y)) = x in y

    type ColorExtensions() = 
        [<Extension>]
        static member ToC4f(this : C3f) = failwith ""
        [<Extension>]
        static member ToC4b(this : C3f) = failwith ""
        [<Extension>]
        static member ToC3b(this : C3f) = failwith ""
        [<Extension>]
        static member ToC3b(this : C4f) = failwith ""
        [<Extension>]
        static member ToC3f(this : C4f) = failwith ""

    type Trafo3d(forward : M44d, backward : M44d) =
        member x.Forward = forward
        member x.Backward = backward

    type Report =

        static member Line(fmt:string, [<ParamArray>] args: Object[]) = failwith ""
        static member Warn(fmt:string, [<ParamArray>] args: Object[]) = failwith ""
        static member BeginTimed(fmt:string, [<ParamArray>] args: Object[]) = failwith ""
        static member Begin(fmt:string, [<ParamArray>] args: Object[]) = failwith ""
        static member End()= failwith ""
        static member End(fmt:string,[<ParamArray>] args: Object[])= failwith ""
        static member Error(fmt:string, [<ParamArray>] args: Object[]) = failwith "" 


    type SortedSetExt<'v>() =
        interface System.Collections.Generic.IEnumerable<'v> with
            member x.GetEnumerator () : System.Collections.Generic.IEnumerator<'v> = failwith ""
            member x.GetEnumerator () : System.Collections.IEnumerator = failwith ""
        member x.FindNeighbours (k:'v, [<Out>]lower : byref<Option<'v>>,[<Out>]self : byref<Option<'v>>,[<Out>]other : byref<Option<'v>>) = failwith ""
        new(cmp : System.Collections.Generic.IComparer<'v>) = SortedSetExt()


    type SortedSetExt with 
        member x.Add v = failwith ""
        member x.Clear () = failwith ""
        member x.Remove v = failwith ""
        member x.FindNeighbours k =
            let self = ref Option<_>.None
            let right = ref Option<_>.None
            let lower = ref Option<_>.None
            x.FindNeighbours(k,lower,self,right)
            !lower,!self,!right
        member x.UnionWith (s : seq<'v>) : unit = failwith ""
        member x.ExceptWith (s : seq<'v>) : unit = failwith ""
        member x.IntersectWith (s : seq<'v>) : unit = failwith ""
        member x.SymmetricExceptWith (s : seq<'v>) : unit = failwith ""
   
    [<AutoOpen>]
    module OptionExt =
        type Option<'a> with
            member x.HasValue = match x with Some v -> true | _ -> false

    type Optional<'a> = Option<'a>

    [<AutoOpen;Extension>]
    module Extensions =

        open System.Linq

        type System.Collections.Generic.Dictionary<'k,'v> with
            member x.TryRemove(k : 'k) =
                if x.ContainsKey k then
                    x.Remove k |> ignore; true, x.[k]
                else false, Unchecked.defaultof<_>
            member x.GetOrCreate(k : 'k, creator : 'k -> 'v) =
                if x.ContainsKey k then x.[k]
                else
                    let v = creator k
                    x.Add(k,v) |> ignore
                    v
            member x.TryAdd(k : 'k, v : 'v) =
                if x.ContainsKey k then false
                else x.Add(k,v); true
        [<Extension>]    
        type ArrayE() =
            [<Extension>]
            static member Map(xs : array<'a>, f) = Array.map f xs
            [<Extension>]
            static member Map(xs : List<'a>, f) = (Seq.map f xs).ToList()

    type Tup<'a,'b>(a : 'a, b : 'b) =
        member x.E0 = a
        member x.E1 = b

    type Vector<'td> = Vector_ of int
    type Vector<'td,'tv> = Vector_ of int

    type VectorExtensions() =
        [<Extension>]
        static member SubVector(this : Vector<'td,'tv>, min,max) : Vector<'td,'tv> = failwith ""
        [<Extension>]
        static member SubVector(this : Vector<'td>, min,max) : Vector<'td> = failwith ""
        [<Extension>]
        static member Size(this : Vector<'td,'tv>) : int64 = failwith ""
        [<Extension>]
        static member Size(this : Vector<'td>) :int64 = failwith ""

    type Matrix<'td> = Vector_ of int
    type Matrix<'td,'tv> = Vector_ of int
    type Matrix<'td> with
        member x.SubVector(min,max) : Vector<'td> = failwith ""
    type Matrix<'td,'tv> with
        member x.SubVector(min,max) : Vector<'td,'tv> = failwith ""

    module Introspection =
        let AllAssemblies : seq<System.Reflection.Assembly> = failwith ""
        let GetAllTypesWithAttribute<'a> () : seq<Tup<System.Type,int>> = failwith ""

    type OnAardvarkInit() = inherit System.Attribute()

    module HashCode =
        let Combine(a,b) = failwith ""

    [<Extension>]
    type EnumerableExtensions =
        [<Extension>]
        static member ToArray(arr : seq<'a>, cnt : int) = arr.ToArray()
        [<Extension>]
        static member SetEquals(a : seq<'a>, b : seq<'a>) : bool = failwith ""

    [<AutoOpen>]
    module LinqEfficiency =
        type System.Collections.Generic.IEnumerable<'a> with
            member x.FirstOrDefault(defaultValue : 'a) = 
                if Seq.isEmpty x then defaultValue
                else Seq.head x

    [<Extension>]
    type AlgodatStuff =
        [<Extension>]
        static member HeapEnqueue(xs : List<'a>,f : System.Func<'a,'a,int>, x : 'a) : unit = failwith ""
        [<Extension>]
        static member HeapDequeue(xs : List<'a>,f : System.Func<'a,'a,int>) : 'a = failwith ""


    type IDict<'TKey, 'TValue> =
       abstract member  Keys : seq<'TKey> with get
       abstract member  Values : seq<'TValue>  with get
       abstract member  KeyValuePairs : IEnumerable<KeyValuePair<'TKey,'TValue>>  with get 
       abstract member  Item : 'TKey -> 'TValue with get,set
       abstract member  Add : key : 'TKey * value : 'TValue -> unit
       abstract member  ContainsKey : key : 'TKey -> bool
       abstract member  Remove : key :'TKey -> bool
       abstract member  TryGetValue : key : 'TKey *  [<Out>] value : byref<'TValue> -> bool

namespace System.Collections.Concurrent 
type ConcurrentHashSet<'k> = System.Collections.Generic.HashSet<'k>