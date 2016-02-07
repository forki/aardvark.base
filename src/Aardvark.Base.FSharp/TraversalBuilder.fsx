#I @"..\..\bin\Debug"

#r "Mono.Reflection.dll"
#r "Aardvark.Base.dll"
#r "Aardvark.Base.Essentials.dll"
#r "Aardvark.Base.TypeProviders.dll"
#r "Aardvark.Base.FSharp.dll"

open System
open System.Threading
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Collections.Concurrent
open Mono.Reflection
open Aardvark.Base
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations


type ILState =
    {
        argTypes : Type[]
        locals : Map<int, LocalBuilder>
        labels : Map<string, Label>
        stack : list<Type>
        generator : ILGenerator

        resetValues : Option<obj[]>
        reset : MethodInfo
    }

type IL<'a> = { build : ILState -> ILState * 'a }

module CodeGenerator =
    
    type Owner =
        class
        end

    let mutable private id = 0
    let private newName() =
        let id = Interlocked.Increment(&id)
        sprintf "__dynamicLambda%d" id


    let private actionTypes =
        [|
            typedefof<System.Action>
            typedefof<System.Action<_>>
            typedefof<System.Action<_,_>>
            typedefof<System.Action<_,_,_>>
            typedefof<System.Action<_,_,_,_>>
            typedefof<System.Action<_,_,_,_,_>>
            typedefof<System.Action<_,_,_,_,_,_>>
            typedefof<System.Action<_,_,_,_,_,_,_>>
        |]

    let private functionTypes =
        [|
            typedefof<System.Func<_>>
            typedefof<System.Func<_,_>>
            typedefof<System.Func<_,_,_>>
            typedefof<System.Func<_,_,_,_>>
            typedefof<System.Func<_,_,_,_,_>>
            typedefof<System.Func<_,_,_,_,_,_>>
            typedefof<System.Func<_,_,_,_,_,_,_>>
            typedefof<System.Func<_,_,_,_,_,_,_,_>>
        |]

    let defineDelegate (args : list<Type>) (ret : Type) (build : ILGenerator -> unit) =
        let args = List.toArray args
        let meth = 
            DynamicMethod(
                newName(),
                MethodAttributes.Public ||| MethodAttributes.Static,
                CallingConventions.Standard,
                ret,args,
                typeof<Owner>,
                true
            )

        let il = meth.GetILGenerator()
        build il

        let delegateType =
            if ret = typeof<unit> then
                actionTypes.[args.Length].MakeGenericType args
            else
                functionTypes.[args.Length].MakeGenericType (Array.append args [|ret|])
                
        meth.CreateDelegate(delegateType)

    let define<'a> (build : ILGenerator -> unit) : 'a =
        
        let args, ret = FunctionReflection.getMethodSignature typeof<'a>
        let args = List.toArray args

        let meth = 
            DynamicMethod(
                newName(),
                MethodAttributes.Public ||| MethodAttributes.Static,
                CallingConventions.Standard,
                ret,args,
                typeof<Owner>,
                true
            )

        let il = meth.GetILGenerator()
        build il
        
        let res = FunctionReflection.buildFunction null meth
        res

    

    let compile<'a> (code : IL<unit>) : 'a =
        
        let args, ret = FunctionReflection.getMethodSignature typeof<'a>
        let args = List.toArray args

        let meth = 
            DynamicMethod(
                newName(),
                MethodAttributes.Public ||| MethodAttributes.Static,
                CallingConventions.Standard,
                ret,args,
                typeof<Owner>,
                true
            )

        let il = meth.GetILGenerator()
        
        let compile() =
            code.build {
                generator = il
                argTypes = args
                locals = Map.empty
                labels = Map.empty
                stack = []
                resetValues = None
                reset = null
            } |> ignore
        
            FunctionReflection.buildFunction null meth
        
        let res = ref (compile())

        !res



[<AutoOpen>]
module ``IL Builder Module`` =
    type ILBuilder() =
        member x.Bind(m : IL<'a>, f : 'a -> IL<'b>) =
            { build = fun s ->
                let s, a = m.build s
                (f a).build s
            }

        member x.Return(v : 'a) =
            { build = fun s -> s, v }

        member x.Yield (v : IL<unit>) =
            v

        member x.Zero() =
            { build = fun s -> s, () }

        member x.For(s : seq<'a>, f : 'a -> IL<unit>) =
            { build = fun state ->
                let mutable state = state
                for e in s do 
                    let s, () = (f e).build state
                    state <- s

                state, ()
            }

        member x.While(guard : unit -> bool, body : IL<unit>) =
            { build = fun state ->
                let mutable state = state
                while guard() do
                    let s,() = body.build state
                    state <- s
                state, ()
            }

        member x.Delay(f : unit -> IL<'a>) =
            { build = fun s ->
                f().build s
            }

        member x.Combine(l : IL<unit>, r : IL<'a>) =
            { build = fun s ->
                let s, () = l.build s
                r.build s
            }

    type RunILBuilder() =
        inherit ILBuilder()

        member x.Run(m : IL<unit>) : 'a =
            CodeGenerator.define (fun il ->
                let state, () =
                    m.build {
                        argTypes = FunctionReflection.getMethodSignature typeof<'a> |> fst |> List.toArray
                        locals = Map.empty
                        labels = Map.empty
                        stack = []
                        generator = il
                        resetValues = None
                        reset = null
                    }

                //state.generator.GetType().GetProperties(BindingFlags.NonPublic ||| BindingFlags.Instance) |> printfn "%A"
                ()
            )


    let il = ILBuilder()
    let cil = RunILBuilder()

    let il' (l : list<IL<unit>>) : 'a =
        CodeGenerator.define (fun il ->
            let mutable state = {
                argTypes = FunctionReflection.getMethodSignature typeof<'a> |> fst |> List.toArray
                locals = Map.empty
                labels = Map.empty
                stack = []
                generator = il
                resetValues = None
                reset = null
            }

            for i in l do
                let s, () = i.build state
                state <- s
        )

    let internal write (f : ILGenerator -> 'a) =
        { build = fun state ->
            let res = f state.generator
            state, Some res
        }


    open TypeInfo.Patterns
    let internal tryBranch (target : Label) mi =
        match mi with
            | MethodQuote <@ (<) : int -> int -> bool @> [t] ->
                match t with
                    | Float32 | Float64 | UInt64 | UInt32 | UInt16 | Byte -> 
                        write (fun il -> il.Emit(OpCodes.Blt_Un, target))
                    | _ ->
                        write (fun il -> il.Emit(OpCodes.Blt, target))

            | MethodQuote <@ (>) : int -> int -> bool @> [t] ->
                match t with
                    | Float32 | Float64 | UInt64 | UInt32 | UInt16 | Byte -> 
                        write (fun il -> il.Emit(OpCodes.Bgt_Un, target))
                    | _ ->
                        write (fun il -> il.Emit(OpCodes.Bgt, target))

            | MethodQuote <@ (<=) : int -> int -> bool @> [t] ->
                match t with
                    | Float32 | Float64 | UInt64 | UInt32 | UInt16 | Byte -> 
                        write (fun il -> il.Emit(OpCodes.Ble_Un, target))
                    | _ ->
                        write (fun il -> il.Emit(OpCodes.Ble, target))

            | MethodQuote <@ (>=) : int -> int -> bool @> [t] ->
                match t with
                    | Float32 | Float64 | UInt64 | UInt32 | UInt16 | Byte -> 
                        write (fun il -> il.Emit(OpCodes.Bge_Un, target))
                    | _ ->
                        write (fun il -> il.Emit(OpCodes.Bge, target))


            | MethodQuote <@ (<>) : int -> int -> bool @> [t] -> 
                match t with
                    | String -> 
                        write (fun il ->
                            il.EmitCall(OpCodes.Call, typeof<System.String>.GetMethod("Equals", [|typeof<string>; typeof<string>|]), null)
                            il.Emit(OpCodes.Brfalse, target)

                        )
                    | _ ->
                        write (fun il -> il.Emit(OpCodes.Bne_Un, target))
            | _ -> 
                il { return None }

    module Map =
        let ofListWithDuplicates (l : list<'a * 'b>) =
            let mutable res = Map.empty

            for (k,v) in l do
                match Map.tryFind k res with
                    | Some old ->
                        res <- Map.add k (v::old) res
                    | None ->
                        res <- Map.add k [v] res
            res



open Aardvark.Base.TypeInfo
type IL private() =
    

    static let rec tryGetMethodInfo (e : Expr) =
        match e with
            | Patterns.Call(_,mi,_) -> 
                if mi.IsGenericMethod then mi |> Some
                else mi |> Some
            | ExprShape.ShapeCombination(_, args) -> 
                args |> List.tryPick tryGetMethodInfo
            | ExprShape.ShapeLambda(_,b) ->
                tryGetMethodInfo b
            | _ -> None

    static let getMethodInfo e =
        match tryGetMethodInfo e with
            | Some mi -> mi
            | None -> failwithf "cannot get method from: %A" e

    static let emit(f : ILGenerator -> 'a) : IL<'a> =
        { build = fun state ->
            let r = f state.generator
            state, r
        }

    static let rec pop n (l : list<'a>) =
        match n, l with
            | 0, l -> l
            | _, [] -> []
            | n, h::t -> pop (n-1) t

    
    static member hash<'a>() =
        let pushInt s = { s with stack = (typeof<uint32>)::s.stack }, ()
        match typeof<'a> with
            | Int32 -> 
                { build = fun s -> 
                    s.generator.Emit(OpCodes.Conv_U4)
                    pushInt s 
                }

            | Int64 -> 
                { build = fun s -> 
                    s.generator.Emit(OpCodes.Conv_U4)
                    pushInt s
                }


            | t when t = typeof<Type> -> 
                { build = fun s -> 
                    s.generator.Emit(OpCodes.Conv_U4)
                    pushInt s
                }

            | t when t.IsClass -> 
                let m = typeof<'a>.GetMethod "GetHashCode"
                { build = fun s -> 
                    s.generator.EmitCall(OpCodes.Callvirt, m, null)
                    s.generator.Emit(OpCodes.Conv_U4)
                    pushInt s
                }
            | t ->
                failwithf "cannot compute hash-code for type: %A" t


    static member mark (l : Label) =
        il {
            do! emit (fun il -> il.MarkLabel l)
        }

    static member newlocal (t : Type) =
        { build = fun state ->
            let l = state.generator.DeclareLocal(t)
            state, l
        }

    static member stloc (l : LocalBuilder) =
        { build = fun state ->
            state.generator.Emit(OpCodes.Stloc, l)
            state, ()
        }

    static member ldarg (i : int) = 
        { build = fun state -> 
            let t = state.argTypes.[i]
            state.generator.Emit(OpCodes.Ldarg, i)
            { state with stack = t::state.stack }, ()
        }

    static member ldnull =
        { build = fun s ->
            s.generator.Emit(OpCodes.Ldnull)
            s, ()
        }

    static member toFloat32 = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_R4)
            state, ()
        }

    static member toFloat64 = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_R8)
            state, ()
        }

    static member print (str : string) =
        { build = fun state -> 
            state.generator.EmitWriteLine str
            state, ()
        }

    static member printTop =
        { build = fun state -> 
            let l = state.generator.DeclareLocal(typeof<obj>)
            state.generator.Emit(OpCodes.Dup)
            state.generator.Emit(OpCodes.Stloc, l)
            state.generator.EmitWriteLine l

            state, ()
        }

    static member print (str : LocalBuilder) =
        { build = fun state -> 
            state.generator.EmitWriteLine str
            state, ()
        }

    static member typeof =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldobj, typeof<nativeint>)
            { state with stack = (typeof<Type>)::state.stack }, ()
        }

    static member ldconst (v : int) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_I4, v)
            { state with stack = (typeof<int>)::state.stack }, ()
        }

    static member ldconst (v : int64) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_I8, v)
            { state with stack = (typeof<int64>)::state.stack }, ()
        }

    static member ldconst (v : nativeint) = 
        if sizeof<nativeint> = 8 then IL.ldconst(v |> int64)
        else IL.ldconst(v |> int32)

    static member ldconst (v : float) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_R8, v)
            { state with stack = (typeof<float>)::state.stack }, ()
        }

    static member ldconst (v : float32) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_R4, v)
            { state with stack = (typeof<float32>)::state.stack }, ()
        }

    static member ldconst (v : string) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldstr, v)
            { state with stack = (typeof<string>)::state.stack }, ()
        }

    static member ldloc (v : LocalBuilder) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldloc, v)
            { state with stack = v.LocalType::state.stack }, ()
        }



    static member dup = 
        { build = fun state -> 
            match state.stack with
                | h::t ->
                    state.generator.Emit(OpCodes.Dup) 
                    { state with stack = h::state.stack }, ()
                | _ ->
                    state.generator.Emit(OpCodes.Dup) 
                    state, ()
        }
    static member ret = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ret) 
            { state with stack = [] }, ()
        }

    static member unbox (a : Type) (t : Type) = 
        { build = fun state -> 
            if a.IsClass && t.IsClass then
                state.generator.Emit(OpCodes.Castclass, t)
            elif a.IsValueType && t = typeof<obj> then
                state.generator.Emit(OpCodes.Box, a)
            
            elif a.IsClass && t.IsValueType then
                state.generator.Emit(OpCodes.Unbox_Any, t)
            else
                let l = state.generator.DeclareLocal(t)
                state.generator.Emit(OpCodes.Stloc, l)
                state.generator.Emit(OpCodes.Ldloc, l)

            { state with stack = [] }, ()
        }

    static member box (t : Type) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Box, t)
            { state with stack = [] }, ()
        }

    static member neg =
        { build = fun state ->
            state.generator.Emit(OpCodes.Neg)
            state, ()
        }

    static member add = 
        { build = fun state -> 
            match state.stack with
                | a::b::stack ->
                    state.generator.Emit(OpCodes.Add)
                    { state with stack = a::stack }, ()
                | _ ->
                    state.generator.Emit(OpCodes.Add)
                    { state with stack = [] }, ()
        }

    static member mul = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Mul)
            state, ()
        }
    static member call (m : MethodInfo) =
        { build = fun state ->
            let mutable args = []
            let mutable stack = state.stack

            for i in 1..m.GetParameters().Length do
                match stack with
                    | a::r ->
                        args <- a::args
                        stack <- r
                    | _ ->
                        ()


            if m.IsVirtual then state.generator.EmitCall(OpCodes.Callvirt, m, null)
            else state.generator.EmitCall(OpCodes.Call, m, null)

            if m.ReturnType = typeof<System.Void> then 
                { state with stack = stack }, ()
            else
                { state with stack = m.ReturnType :: stack }, ()
        }

    static member call (m : Expr) =
        let m = getMethodInfo m
        IL.call m

    static member newlabel = { build = fun state -> state, state.generator.DefineLabel() }

    static member jmp (l : Label) =
        il {
            do! emit (fun il -> il.Emit(OpCodes.Br, l))
        }

    static member jmpIf (l : Label) (e : Expr<'a -> 'b -> bool>) =
        let m = getMethodInfo e 

        { build = fun state ->
            match (tryBranch l m).build state with
                | (state, Some ()) ->   
                    { state with stack = pop 2 state.stack }, ()
                | _ ->
                    failwith "asdölsamdklm,asdl"
        }
          

    static member ldimm (v : 'a) =
        match v :> obj with
            | :? int as v -> IL.ldconst v
            | :? int64 as v -> IL.ldconst v
            | :? nativeint as v -> IL.ldconst v
            | :? string as v -> IL.ldconst v
            | :? Type as v -> IL.ldconst(v.TypeHandle.Value)
            | _ -> failwithf "cannot load immediate value: %A" v

    static member rebuildAndRun =
        { build = fun state ->
            if isNull state.reset then
                failwith "cannot reset dynamic code here"

            let il = state.generator

            il.Emit(OpCodes.Ldarg, state.argTypes.Length-1)

            for i in 0..state.argTypes.Length-2 do
                il.Emit(OpCodes.Ldarg, i)

            il.EmitCall(OpCodes.Callvirt, state.reset, null)
            state, ()
        }

    static member args = 
        { build = fun state ->
            state, state.resetValues.Value
        }

    static member jmpTable (alternatives : list<'a * IL<unit>>) : IL<unit> =
        
        let hashCode =
            if typeof<'a> = typeof<Type> then fun (a : 'a) -> (unbox<Type> a).TypeHandle.Value |> uint32
            elif typeof<'a> = typeof<int> then unbox<int> >> uint32
            elif typeof<'a> = typeof<int64> then unbox<int64> >> uint32
            else fun (a : 'a) -> a.GetHashCode() |> uint32
                

        { build = fun state ->
            let mutable state = state

            let smallestHash = ref UInt32.MaxValue
            let data =
                alternatives 
                    |> List.map (fun (v,c) ->
                        let hash = uint32 (hashCode v)
                    
                        if hash < !smallestHash then
                            smallestHash := hash

                        hash, (v,c)
                    )
                    |> List.map (fun (k,v) -> (k - !smallestHash),v)
                    |> Map.ofListWithDuplicates

            let smallestHash = !smallestHash

            let buildMatchTable (size : uint32) (data : Map<uint32, list<'a * IL<unit>>>) =
                let arr = Array.create (int size) []
                let mutable duplicates = 0

                for (hash,values) in Map.toSeq data do
                    let id = int (hash % size)
                    arr.[id] <- 
                        match arr.[id] with
                            | [] -> values
                            | r -> 
                                duplicates <- duplicates + 1
                                values @ r

                
                let score = 1.0 / (1.0 + float duplicates)
                score, arr

            let size = data.Count
            let tables =
                Seq.init (2 * size) (fun i ->
                    let s = uint32 (size + i)
                    buildMatchTable s data
                )
                |> Seq.cache

            let gen = state.generator

            let table = 
                match tables |> Seq.tryFind (fun (s,_) -> s >= 1.0) with
                    | Some (_,perfect) -> perfect
                    | None ->
                        let _,best = tables |> Seq.minBy fst
                        best

            let elseLabel = gen.DefineLabel()

            let labels = 
                table |> Array.map (fun l ->
                    match l with
                        | [] -> gen.DefineLabel()//elseLabel
                        | _ -> gen.DefineLabel()
                )

            let v = gen.DeclareLocal(typeof<'a>)
            gen.Emit(OpCodes.Stloc, v)

            
            // it = it.GetHashCode()
            gen.Emit(OpCodes.Ldloc, v)
            let s, () = IL.hash<'a>().build state
            state <- s

            // it = it - smallestHash
            gen.Emit(OpCodes.Ldc_I8, int64 smallestHash)
            gen.Emit(OpCodes.Conv_U4)
            gen.Emit(OpCodes.Sub)

            // it = it % tableSize
            gen.Emit(OpCodes.Ldc_I8, int64 labels.Length)
            gen.Emit(OpCodes.Conv_U4)
            gen.Emit(OpCodes.Rem)

            gen.Emit(OpCodes.Switch, labels)
            gen.Emit(OpCodes.Br, elseLabel)

            for i in 0..table.Length-1 do

                gen.MarkLabel(labels.[i])

                let alternatives = table.[i]

                for (value,code) in alternatives do
                    let labelName = Guid.NewGuid().ToString()
                    let code =
                        il {
                            do! IL.ldimm value
                            do! IL.ldloc v
                            let! l = IL.newlabel
                            do! IL.jmpIf l <@ (<>) : 'a -> 'a -> bool @>

                            do! code

                            do! IL.mark l
                        }

                    let s, () = code.build state
                    state <- s

                gen.Emit(OpCodes.Br, elseLabel)



            gen.MarkLabel(elseLabel)

            state, ()
        }


    static member generator =
        { build = fun il -> il, il.generator }


module Test =

    let test : float -> obj -> float =
        cil {
            // load arg and 10.0 onto the stack
            do! IL.ldarg 0

            do! IL.ldconst 10.0


            // compute a minimum of both values
            do! IL.call <@ Fun.Min : float * float -> float @>

            // square the value
            do! IL.ldconst 2.0
            do! IL.call <@ Fun.Pow : float * float -> float @>

            // get the type of argument 1
            do! IL.ldarg 1
            do! IL.typeof

            do! IL.jmpTable [

                    typeof<int>, 
                        il {
                            do! IL.ldarg 1
                            do! IL.unbox typeof<obj> typeof<int>
                            do! IL.toFloat64
                            
                            do! IL.print "int"

                            do! IL.add
                            do! IL.ret
                        }

                    typeof<float>,
                        il {
                            do! IL.ldarg 1
                            do! IL.unbox typeof<obj> typeof<float>
                            do! IL.print "float"

                            do! IL.add
                            do! IL.ret
                        }
                        
                ]


            do! IL.ret
        }

    let testSemantic : string -> obj -> obj =
        cil {
            do! IL.ldarg 0
            do! IL.jmpTable [
                    "Sum",
                        il {
                            do! IL.ldarg 1
                            do! IL.typeof

                            do! IL.jmpTable [
                                    typeof<int>, 
                                        il { 
                                            do! IL.ldarg 1
                                            do! IL.ret
                                        }

                                    typeof<float>, 
                                        il { 
                                            do! IL.ldarg 1
                                            do! IL.ret
                                        }

                                    typeof<list<int>>,
                                        il {
                                            do! IL.ldarg 1

                                            do! IL.unbox typeof<obj> typeof<list<int>>
                                            do! IL.call <@ List.sum : list<int> -> int @>
                                            do! IL.unbox typeof<int> typeof<obj>

                                            do! IL.ret
                                        }

                                    typeof<list<float>>,
                                        il {
                                            do! IL.ldarg 1

                                            do! IL.unbox typeof<obj> typeof<list<float>>
                                            do! IL.call <@ List.sum : list<float> -> float @>
                                            do! IL.unbox typeof<float> typeof<obj>

                                            do! IL.ret
                                        }

                                ]

                            do! IL.ldnull
                            do! IL.ret
                        }
                ]

            do! IL.ldnull
            do! IL.ret 

        }

    type ChangeableFunc<'a,'b>(f : IL<unit>) =
        inherit FSharpFunc<'a, 'b>()
        let mutable cache = None


        let rebuild(cause : Option<obj[]>) =
            let del = 
                CodeGenerator.defineDelegate [typeof<'a>; typeof<ChangeableFunc<'a, 'b>>] typeof<'b> (fun il ->
                    f.build {
                        generator = il
                        labels = Map.empty
                        locals = Map.empty
                        stack = []
                        argTypes = [| typeof<'a>; typeof<ChangeableFunc<'a, 'b>> |]
                        resetValues = cause
                        reset = typeof<ChangeableFunc<'a, 'b>>.GetMethod "Reset"
                    } |> ignore
                ) |> unbox<Func<'a, ChangeableFunc<'a, 'b>, 'b>>

            cache <- Some del
            del

        let get(value : 'a) =
            match cache with
                | Some c -> c
                | None -> rebuild (Some [|value :> obj|])

        member x.Reset(value : 'a) =
            let c = rebuild (Some [|value :> obj|])
            c.Invoke(value, x)


        override x.Invoke(value : 'a) =
            get(value).Invoke(value, x)

    type ChangeableFunc<'a, 'b, 'c>(f : IL<unit>) =
        inherit Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc<'a, 'b, 'c>()
        let mutable cache = None
        let l = System.Threading.SpinLock()
        let mutable taken = false

        let rebuild(cause : Option<obj[]>) =
            let del = 
                CodeGenerator.defineDelegate [typeof<'a>; typeof<'b>; typeof<ChangeableFunc<'a, 'b, 'c>>] typeof<'c> (fun il ->
                    f.build {
                        generator = il
                        labels = Map.empty
                        locals = Map.empty
                        stack = []
                        argTypes = [| typeof<'a>; typeof<'b>; typeof<ChangeableFunc<'a, 'b, 'c>> |]
                        resetValues = cause
                        reset = typeof<ChangeableFunc<'a, 'b, 'c>>.GetMethod "Reset"
                    } |> ignore
                ) |> unbox<Func<'a, 'b, ChangeableFunc<'a, 'b, 'c>, 'c>>

            cache <- Some del
            del

        let get(a : 'a, b : 'b) =
            match cache with
                | Some c -> c
                | None -> rebuild (Some [|a :> obj; b :> obj|])

        member x.Reset(a : 'a, b : 'b) =
            try
                if taken then
                    failwith "nested rebuild"
                l.Enter(&taken)
                let c = rebuild (Some [|a :> obj; b :> obj|])
                c.Invoke(a, b, x)
            finally
                if taken then 
                    taken <- false
                    l.Exit()

        override x.Invoke(a : 'a) =
            fun b -> x.Invoke(a,b)

        override x.Invoke(a : 'a, b : 'b) =
            get(a,b).Invoke(a, b, x)


    type DynamicILBuilder() =
        inherit ILBuilder()

        let fTypes =
            [|
                null
                null
                typedefof<ChangeableFunc<_,_>>
                typedefof<ChangeableFunc<_,_,_>>
            |]

        member x.Run(f : IL<unit>) : 'a =
            let args, ret = FunctionReflection.getMethodSignature typeof<'a>

            let targs = args @ [ret] |> List.toArray
            let ftype = fTypes.[targs.Length].MakeGenericType targs

            Activator.CreateInstance(ftype, f) |> unbox<_>

    let dynil = DynamicILBuilder()

    let fDyn (methods : 'a -> Option<IL<unit>>) : 'a -> 'b -> 'c =
        let cases = ref []
        
        dynil {
            let! a = IL.args
            let cid = unbox a.[0]
            let! nothing = IL.newlabel

            match methods cid with
                | Some m ->
                    let code =
                        il {
                            do! IL.ldarg 1
                            do! m
                            do! IL.ret
                        }
                    cases := (cid, code)::!cases
                | _ ->
                    cases := (cid, il { do! IL.jmp nothing })::!cases
 
            do! IL.ldarg 0
            do! IL.jmpTable !cases
            do! IL.rebuildAndRun
            do! IL.ret

            do! IL.mark nothing
            do! IL.ldnull
            do! IL.ret
        }


    let dispatcher : int -> int -> int =
        fDyn (fun name ->
            printfn "looking up %A" name
            if name % 2 = 0 then
                il { 
                    do! IL.ldconst name
                    do! IL.add
                } |> Some
            else
                None
//            match name with
//                | "abs" -> il { do! IL.call <@ Fun.Abs : int -> int @> } |> Some
//                | "neg" -> il { do! IL.neg } |> Some
//                | "dbl" -> 
//                    il { 
//                        do! IL.dup
//                        do! IL.add 
//                    } |> Some
//                | _ -> None
        )