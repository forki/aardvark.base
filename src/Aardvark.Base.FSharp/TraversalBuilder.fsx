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
open Aardvark.Base.TypeInfo

type ILState =
    {
        argTypes : Type[]
        retType : Type
        stack : list<Type>
        generator : ILGenerator

        reachable : bool
        labelStacks : HashMap<Label, list<Type>>

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
                retType = ret
                stack = []
                reachable = true
                labelStacks = HashMap.empty
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
                    let args, ret = FunctionReflection.getMethodSignature typeof<'a>
                    m.build {
                        argTypes = args |> List.toArray
                        retType = ret
                        stack = []
                        generator = il
                        reachable = true
                        labelStacks = HashMap.empty
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
            let args, ret = FunctionReflection.getMethodSignature typeof<'a>
            let mutable state = {
                argTypes = args |> List.toArray
                retType = ret
                stack = []
                generator = il
                reachable = true
                labelStacks = HashMap.empty
                resetValues = None
                reset = null
            }

            for i in l do
                let s, () = i.build state
                state <- s
        )




[<AutoOpen>]
module private ILHelpers =
    open TypeInfo.Patterns

    let write (f : ILGenerator -> 'a) =
        { build = fun state ->
            let res = f state.generator
            state, Some res
        }


    let tryBranch (target : Label) mi =
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

            | MethodQuote <@ (=) : int -> int -> bool @> [t] -> 
                match t with
                    | String -> 
                        write (fun il ->
                            il.EmitCall(OpCodes.Call, typeof<System.String>.GetMethod("Equals", [|typeof<string>; typeof<string>|]), null)
                            il.Emit(OpCodes.Brtrue, target)
                        )
                    | _ ->
                        write (fun il -> il.Emit(OpCodes.Beq, target))

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


type IL private() =
    

    static let rec tryGetMethodInfo (e : Expr) =
        match e with
            | Patterns.Call(_,mi,_) -> 
                if mi.IsGenericMethod then mi |> Some
                else mi |> Some

            | Patterns.NewUnionCase(c, _) ->
                c.DeclaringType.GetMethod c.Name |> Some

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

    static let spush t (state,r) =
        { state with stack = t::state.stack },r

    static let spop n (state,r) =
        let rec popn n l =
            match n, l with
            | 0, l -> l
            | _, [] -> []
            | n, h::t -> popn (n-1) t

        { state with stack = popn n state.stack },r

    static let speek n state =
        let rec types n l =
            if n > 0 then
                match l with
                    | h::t -> h::(types (n - 1) t)
                    | [] -> List.init n (fun _ -> typeof<obj>)
            else
                []

        types n state.stack

    static let speek1 state =
        let rec types n l =
            match n,l with
                | 0,_ -> []
                | n,h::t -> h::(types (n - 1) t)
                | n, [] -> (typeof<obj>)::(types (n - 1) l)
        types 1 state.stack |> List.head

    static let srep t (state,r) =
        { state with stack = t::(List.tail state.stack)},r

    static let modifyState f =
        { build = fun state ->
            (f state), ()
        }

    static let readState f =
        { build = fun state ->
            state, f state
        }



    static member hash<'a>() =
        match typeof<'a> with
            | Int32 -> 
                { build = fun s -> 
                    (s, ()) |> srep typeof<uint32>
                }

            | Int64 -> 
                { build = fun s -> 
                    s.generator.Emit(OpCodes.Conv_U4)
                    (s, ()) |> srep typeof<uint32>
                }

            | t when t = typeof<Type> -> 
                { build = fun s -> 
                    s.generator.Emit(OpCodes.Conv_U4)
                    (s, ()) |> srep typeof<uint32>
                }

            | t when t.IsClass -> 
                let m = typeof<'a>.GetMethod "GetHashCode"
                { build = fun s -> 
                    s.generator.EmitCall(OpCodes.Callvirt, m, null)
                    (s, ()) |> srep typeof<uint32>
                }

            | t ->
                let m = typeof<obj>.GetMethod "GetHashCode"
                { build = fun s -> 
                    s.generator.Emit(OpCodes.Box, typeof<obj>)
                    s.generator.EmitCall(OpCodes.Callvirt, m, null)
                    (s, ()) |> srep typeof<uint32>
                }

    static member ldarg (i : int) = 
        { build = fun state -> 
            let t = state.argTypes.[i]
            state.generator.Emit(OpCodes.Ldarg, i)
            (state, ()) |> spush t
        }

    static member ldnull =
        { build = fun s ->
            s.generator.Emit(OpCodes.Ldnull)
            (s, ()) |> spush typeof<obj>
        }



    static member toFloat32 = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_R4)
            (state, ()) |> srep typeof<float32>
        }

    static member toFloat64 = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_R8)
            (state, ()) |> srep typeof<float>
        }

    static member toInt8 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_I1)
            (state, ()) |> srep typeof<int8>
        }

    static member toInt16 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_I2)
            (state, ()) |> srep typeof<int16>
        }

    static member toInt32 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_I4)
            (state, ()) |> srep typeof<int32>
        }

    static member toInt64 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_I8)
            (state, ()) |> srep typeof<int64>
        }

    static member toNativeInt =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_I)
            (state, ()) |> srep typeof<nativeint>
        } 

    static member toUInt8 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_U1)
            (state, ()) |> srep typeof<uint8>
        }

    static member toUInt16 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_U2)
            (state, ()) |> srep typeof<uint16>
        }

    static member toUInt32 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_U4)
            (state, ()) |> srep typeof<uint32>
        }

    static member toUInt64 =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_U8)
            (state, ()) |> srep typeof<uint64>
        }

    static member toUNativeInt =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Conv_U)
            (state, ()) |> srep typeof<unativeint>
        } 



    static member private printStackArr (arr : obj[]) =
        printfn "stack: [%s]" (arr |> Array.map (sprintf "%A") |> String.concat "; ")

    static member private printNamed (name : string, value : obj) =
        printfn "%s: %A" name value

    static member print (str : string) =
        { build = fun state -> 
            state.generator.EmitWriteLine str
            state, ()
        }

    static member printstack (n : int) =
        { build = fun state ->
            
            let il = state.generator
            let locals = Array.init n (fun i -> il.DeclareLocal(state.stack.[i]))
            for i in 0..n-1 do
                il.Emit(OpCodes.Stloc, locals.[i])

            for i in 1..n do
                il.Emit(OpCodes.Ldloc, locals.[n - i])

            
            let arr = il.DeclareLocal(typeof<obj[]>)
            il.Emit(OpCodes.Ldc_I4, n)
            il.Emit(OpCodes.Newarr, typeof<obj>)
            il.Emit(OpCodes.Stloc, arr)

            for i in 0..n-1 do
                let l = locals.[i]

                il.Emit(OpCodes.Ldloc, arr)
                il.Emit(OpCodes.Ldc_I4, i)

                il.Emit(OpCodes.Ldloc, l)
                if l.LocalType <> typeof<obj> then
                    if l.LocalType.IsClass then il.Emit(OpCodes.Unbox_Any, typeof<obj>)
                    else il.Emit(OpCodes.Box, typeof<obj>)

                il.Emit(OpCodes.Stelem_Ref)

            il.Emit(OpCodes.Ldloc, arr)
            il.EmitCall(OpCodes.Call, typeof<IL>.GetMethod("printStackArr", BindingFlags.NonPublic ||| BindingFlags.Static), null)

//            il.EmitWriteLine(arr)

//            il.Emit(OpCodes.Ldstr, "; ")
//
//            il.Emit(OpCodes.Ldc_I4, n)
//            il.Emit(OpCodes.Newarr, typeof<string>)
//
//            for i in 0..n-1 do
//                il.Emit(OpCodes.Dup)
//                il.Emit(OpCodes.Ldc_I4, i)
//
//                il.Emit(OpCodes.Ldloc, locals.[i])
//                il.EmitCall(OpCodes.Callvirt, locals.[i].LocalType.GetMethod("ToString", [||]), null)
//
//                il.Emit(OpCodes.Stelem)
//
//            il.Emit(OpCodes.Unbox_Any, typeof<seq<string>>)
//            il.EmitCall(OpCodes.Call, getMethodInfo <@ String.concat @>, null)
//            let str = il.DeclareLocal(typeof<string>)
//            il.Emit(OpCodes.Stloc, str)
//            il.EmitWriteLine(str)

            state, ()

        }

    static member print (name : string, l : LocalBuilder) =
        { build = fun state -> 
            
            let il = state.generator

            il.Emit(OpCodes.Ldstr, name)
            il.Emit(OpCodes.Ldloc, l)
            let state, () = (IL.convert l.LocalType typeof<obj>).build state
            il.EmitCall(OpCodes.Call, getMethodInfo <@ IL.printNamed @>, null)

            state, ()
        }



    static member typeof =
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldobj, typeof<nativeint>)
            (state, ()) |> srep typeof<Type>
        }

    static member ldconst (v : int) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_I4, v)
            (state, ()) |> spush typeof<int>
        }

    static member ldconst (v : int64) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_I8, v)
            (state, ()) |> spush typeof<int64>
        }

    static member ldconst (v : nativeint) = 
        if sizeof<nativeint> = 8 then IL.ldconst(v |> int64)
        else IL.ldconst(v |> int32)

    static member ldconst (v : float32) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_R4, v)
            (state, ()) |> spush typeof<float32>
        }

    static member ldconst (v : float) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldc_R8, v)
            (state, ()) |> spush typeof<float>
        }

    static member ldconst (v : string) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldstr, v)
            (state, ()) |> spush typeof<string>
        }

    static member ldval (v : 'a) =
        match v :> obj with
            | :? int as v -> IL.ldconst v
            | :? int64 as v -> IL.ldconst v
            | :? nativeint as v -> IL.ldconst v
            | :? string as v -> IL.ldconst v
            | :? Type as v -> IL.ldconst(v.TypeHandle.Value)
            | _ -> failwithf "cannot load immediate value: %A" v


    static member newlocal (t : Type) =
        { build = fun state ->
            let l = state.generator.DeclareLocal(t)
            state, l 
        }

    static member ldloc (v : LocalBuilder) = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ldloc, v)
            (state, ()) |> spush v.LocalType
        }

    static member stloc (l : LocalBuilder) =
        { build = fun state ->
            state.generator.Emit(OpCodes.Stloc, l)
            (state, ()) |> spop 1
        }



    static member dup = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Dup) 
            let h = speek1 state
            (state, ()) |> spush h
        }

    static member pop =
        { build = fun state ->
            state.generator.Emit(OpCodes.Pop)
            (state, ()) |> spop 1
        }

    static member ret = 
        { build = fun state -> 
            state.generator.Emit(OpCodes.Ret) 
            ({ state with reachable = false }, ()) |> spop 1
        }

    static member convert (a : Type) (t : Type) = 
        { build = fun state -> 
            match a.IsClass, t.IsClass with
                | true, true ->
                    state.generator.Emit(OpCodes.Unbox_Any, t)
                | true, false ->
                    state.generator.Emit(OpCodes.Unbox_Any, t)
                | false, true ->
                    state.generator.Emit(OpCodes.Box, a)  
                | false, false ->
                    let l = state.generator.DeclareLocal(t)
                    state.generator.Emit(OpCodes.Stloc, l)
                    state.generator.Emit(OpCodes.Ldloc, l)


            (state,()) |> srep t
        }



    static member neg =
        { build = fun state ->
            state.generator.Emit(OpCodes.Neg)
            state, ()
        }

    static member add = 
        { build = fun state -> 
            let t = speek1 state
            state.generator.Emit(OpCodes.Add)
            (state, ()) |> spop 2 |> spush t
        }

    static member sub = 
        { build = fun state -> 
            let t = speek1 state
            state.generator.Emit(OpCodes.Sub)
            (state, ()) |> spop 2 |> spush t
        }

    static member mul = 
        { build = fun state -> 
            let t = speek1 state
            state.generator.Emit(OpCodes.Mul)
            (state, ()) |> spop 2 |> spush t
        }

    static member div = 
        { build = fun state -> 
            let t = speek1 state
            match t with
                | Byte | UInt16 | UInt32 | UInt64 -> 
                    state.generator.Emit(OpCodes.Div_Un)
                | _ ->
                    state.generator.Emit(OpCodes.Div)
            (state, ()) |> spop 2 |> spush t
        }

    static member rem = 
        { build = fun state -> 
            let t = speek1 state
            match t with
                | Byte | UInt16 | UInt32 | UInt64 -> 
                    state.generator.Emit(OpCodes.Rem_Un)
                | _ ->
                    state.generator.Emit(OpCodes.Rem)
            (state, ()) |> spop 2 |> spush t
        }



    static member call (m : MethodInfo) =
        { build = fun state ->
            let parameters = m.GetParameters()

            let cnt = 
                if m.IsStatic then parameters.Length
                else 1 + parameters.Length

            if m.IsVirtual then state.generator.EmitCall(OpCodes.Callvirt, m, null)
            else state.generator.EmitCall(OpCodes.Call, m, null)

            if m.ReturnType = typeof<System.Void> then 
                (state, ()) |> spop cnt
            else
                (state, ()) |> spop cnt |> spush m.ReturnType
        }

    static member call (m : Expr) =
        let m = getMethodInfo m
        IL.call m

    static member getProp (m : PropertyInfo) =
        IL.call m.GetMethod

    static member getField (m : FieldInfo) =
        { build = fun state ->
            state.generator.Emit(OpCodes.Ldfld, m)
            (state, ()) |> srep m.FieldType
        }

    static member label = { build = fun state -> state, state.generator.DefineLabel() }

    static member mark (l : Label) =
        emit (fun il -> il.MarkLabel l)

    static member jmp (l : Label) =
        { build = fun state ->
            state.generator.Emit(OpCodes.Br, l)
            { state with 
                labelStacks = HashMap.add l state.stack state.labelStacks 
                reachable = false
            }, ()
        }

    static member jmpIf (l : Label) (e : Expr<'a -> 'b -> bool>) =
        let m = getMethodInfo e 

        { build = fun state ->
            match (tryBranch l m).build state with
                | (state, Some ()) ->   
                    let state = { state with labelStacks = HashMap.add l state.stack state.labelStacks }
                    (state, ()) |> spop 2
                | _ ->
                    failwithf "unknown branch condition: %A" e
        }
          

    static member rebuildAndRun =
        { build = fun state ->
            if isNull state.reset then
                failwith "cannot reset dynamic code here"

            let il = state.generator

            il.Emit(OpCodes.Ldarg, state.argTypes.Length-1)

            for i in 0..state.argTypes.Length-2 do
                il.Emit(OpCodes.Ldarg, i)

            il.EmitCall(OpCodes.Callvirt, state.reset, null)
            { state with stack = [state.retType] }, ()
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
            elif typeof<'a> = typeof<string> then fun (a : 'a) -> (unbox<string> a).GetHashCode() |> uint32
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
                    | Some (_,perfect) -> 
                        perfect
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
            gen.Emit(OpCodes.Rem_Un)

            gen.Emit(OpCodes.Switch, labels)
            gen.Emit(OpCodes.Br, elseLabel)

            for i in 0..table.Length-1 do

                gen.MarkLabel(labels.[i])

                let alternatives = table.[i]

                for (value,code) in alternatives do
                    let labelName = Guid.NewGuid().ToString()
                    let code =
                        il {
                            let! l = IL.label
                            do! IL.ldval value
                            do! IL.ldloc v
                            //do! IL.printstack 2

                            do! IL.jmpIf l <@ (<>) : 'a -> 'a -> bool @>

                            do! code

                            do! IL.mark l
                            
                        }

                    let s, () = code.build { state with stack = state.stack }
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
                            do! IL.convert typeof<obj> typeof<int>
                            do! IL.toFloat64
                            
                            do! IL.print "int"

                            do! IL.add
                            do! IL.ret
                        }

                    typeof<float>,
                        il {
                            do! IL.ldarg 1
                            do! IL.convert typeof<obj> typeof<float>
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

                                            do! IL.convert typeof<obj> typeof<list<int>>
                                            do! IL.call <@ List.sum : list<int> -> int @>
                                            do! IL.convert typeof<int> typeof<obj>

                                            do! IL.ret
                                        }

                                    typeof<list<float>>,
                                        il {
                                            do! IL.ldarg 1

                                            do! IL.convert typeof<obj> typeof<list<float>>
                                            do! IL.call <@ List.sum : list<float> -> float @>
                                            do! IL.convert typeof<float> typeof<obj>

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
                        stack = []
                        argTypes = [| typeof<'a>; typeof<ChangeableFunc<'a, 'b>> |]
                        retType = typeof<'b>
                        reachable = true
                        labelStacks = HashMap.empty
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
                        stack = []
                        argTypes = [| typeof<'a>; typeof<'b>; typeof<ChangeableFunc<'a, 'b, 'c>> |]
                        retType = typeof<'c>
                        reachable = true
                        labelStacks = HashMap.empty
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

    let fDyn (methods : 'a -> Label -> IL<unit>) : 'a -> 'b -> 'c =
        let cases = ref []
        
        dynil {
            let! a = IL.args
            let cid = unbox a.[0]
            let! nothing = IL.label
            let code =
                il {
                    do! IL.ldarg 1
                    do! methods cid nothing
                    do! IL.ret
                }
            cases := (cid, code)::!cases
 
            do! IL.ldarg 0
            do! IL.jmpTable !cases
            do! IL.rebuildAndRun
            do! IL.ret

            do! IL.mark nothing
            do! IL.pop
            do! IL.ldnull
            do! IL.ret
        }

    let fDyn2 (methods : 'a -> Type -> IL<unit>) : 'a -> obj -> 'c =
        let cases = Dict<'a, ref<list<Type * IL<unit>>>>()
        
        dynil {
            let! a = IL.args
            let aid = unbox a.[0]
            let bid = (unbox a.[1]).GetType()

            if bid.IsArray then
                failwithf "cannot use type-tests on arrays atm."

            let! nothing = IL.label
            let code =
                il {
                    do! IL.ldarg 1
                    do! IL.convert typeof<obj> bid
                    do! methods aid bid
                    
                }

            let r = cases.GetOrCreate(aid, fun aid -> ref [])
            r := (bid, code)::!r
 

            do! IL.ldarg 0
            do! cases 
                |> Dict.toList 
                |> List.map (fun (a,inner) -> 
                    let code =
                        il {
//                            let! l = IL.newlocal typeof<nativeint>
//                            do! IL.ldarg 1
//                            do! IL.typeof
//                            do! IL.stloc l
//                            do! IL.print("type", l)

                            do! IL.ldarg 1
                            do! IL.typeof
                            do! IL.jmpTable !inner


                        }
                    a,code
                   )
                |> IL.jmpTable

            do! IL.rebuildAndRun
            do! IL.ret

        }

    let dispatcher : string -> int -> int =
        fDyn (fun name error ->
            il {
                printfn "looking up %A" name
    
                match name with
                    | "abs" -> 
                        do! IL.call <@ Fun.Abs : int -> int @>

                    | "neg" -> 
                        do! IL.neg

                    | "dbl" -> 
                        do! IL.dup
                        do! IL.add 
                       
                    | _ ->
                        do! IL.jmp error
            }
        )

    type MyList<'a> = interface end
    type MyCons<'a> =
        class 
            val mutable public Head : 'a
            val mutable public Tail : MyList<'a>

            interface MyList<'a>
            new(head : 'a, tail : MyList<'a>) = { Head = head; Tail = tail }
        end 
        

    type MyNil<'a>() =
        interface MyList<'a>



    [<System.Runtime.CompilerServices.MethodImplAttribute(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let rec mysum (l : MyList<int>) =
        match l with
            | :? MyCons<int> as c -> c.Head + mysum c.Tail
            | :? MyNil<int> as n -> 0
            | _ -> 0


    
    type Ag() =
        static let ag : string -> obj -> int =
            fDyn2 (fun name t ->
                il {
                    if name = "Sum" then
                        if t = typeof<MyNil<int>> then
                            do! IL.pop
                            do! IL.ldconst 0
                            do! IL.ret
                        elif t = typeof<MyCons<int>> then
                            let! l = IL.newlocal typeof<MyCons<int>>
                            do! IL.stloc l

                            do! IL.ldloc l
                            do! IL.getField (typeof<MyCons<int>>.GetField "Head")

                            do! IL.ldconst name
                            do! IL.ldloc l
                            do! IL.getField (typeof<MyCons<int>>.GetField "Tail")
                            //do! IL.convert l.LocalType typeof<obj>
                            do! IL.call <@ Ag.Eval : string -> obj -> int @>

                            do! IL.add
                            do! IL.ret
                        else
                            do! IL.pop
                            do! IL.ldconst 0
                            do! IL.ret
                    else
                        do! IL.pop
                        do! IL.ldconst 0
                        do! IL.ret
//                    match name, t with
//                        | "Sum", Int32 -> 
//                            do! IL.call <@ Some : int -> _ @>
//                            do! IL.ret
//
//                        | "Sum", List(Int32) ->
//                            do! IL.call <@ Ag.Eval @>
//                            do! IL.ret
//
//                        | "Min", Int32 -> 
//                            do! IL.call <@ Some : int -> _ @>
//                            do! IL.ret
//
//                        | "Min", List(Int32) ->
//                            do! IL.call <@ List.min : list<int> -> int @>
//                            do! IL.call <@ Some : int -> _ @>
//                            do! IL.ret
//
//                        | _ ->
//                            do! IL.pop
//                            do! IL.ldnull
//                            do! IL.ret
                        
                }
            )

        static member Eval a b = ag a b

    let ensure (name : string) (v : int) (e : int) =
        let r = dispatcher name v
        if e <> r then
            failwithf "dispatcher %A %A returned %A (expected %A)" name v r e


    let testDispatcher() =
        ensure "abs" -1 1
        ensure "dbl" -1 -2
        ensure "neg" -1 1
        ensure "bla" -1 0
        ensure "blubb" -1 0

        
        ensure "abs" -1 1
        ensure "dbl" -1 -2
        ensure "neg" -1 1
        ensure "bla" -1 0
        ensure "blubb" -1 0

    let stringdisp : string -> string -> string =
        fDyn (fun str err ->
            il {
                do! IL.ldconst str
                do! IL.call <@ (+) : string -> string -> string @>
            }
        )

    let printing : string -> obj -> obj =
        cil {
            do! IL.ldarg 0
            do! IL.ldarg 1
            do! IL.printstack 2
            do! IL.pop
            do! IL.pop

            do! IL.ldarg 1
            do! IL.ret
        }


    open System.Diagnostics
    open System.IO
    let logFile = Path.combine [Environment.GetFolderPath(Environment.SpecialFolder.Desktop); "agperf.csv"]

    let testAgPerf(size : int) =
        let myList (size : int) (f : int -> int) =
            let rec build (i : int) =
                if i >= size then MyNil<int>() :> MyList<int>
                else
                    let v = f i
                    MyCons(v, build (i+1)):> MyList<int>
            build 0

        let data = myList size id
        let iter = 1 <<< 18

        // 16 types
        let garbageIn = 
            [
                data :> obj
                obj()
                List<int>() :> obj
                Some 1 :> obj
                "asd" :> obj
                (1,2) :> obj
                (1,2,3) :> obj
                (fun a -> a) :> obj
                (HashSet<int>()) :> obj
                (Dictionary<int, int>()) :> obj
                (Dictionary<int, float>()) :> obj
                (Dictionary<float, float>()) :> obj
                (Dictionary<int64, float>()) :> obj
                (Dictionary<int64, int64>()) :> obj
                ([1.0]) :> obj
                ([0uy]) :> obj
            ]

        // 8 semantics
        let sem = ["Sum"; "Abs"; "Min"; "b"; "c"; "d"; "e"; "f"]

        // use ag with all combinations of types and names
        for i in 1..100 do
            for s in sem do
                for g in garbageIn do
                    Ag.Eval s g |> ignore

        // warmup List.sum
        for i in 1..100 do
            mysum data |> ignore


        let expected = data |> mysum
        let check str r =
            if r <> expected then
                failwithf "unexpected result from %A: %A" str r

        Ag.Eval "Sum" data |> check "AG"
        data |> mysum |> check "Raw"

        // benchmark Sum(list<int>)
        let sw = Stopwatch()
        sw.Start()
        for i in 1..iter do
            Ag.Eval "Sum" data |> ignore
        sw.Stop()
        let tag = (1000.0 * sw.Elapsed.TotalMilliseconds / float iter)
        printfn "ag:  %.3fµs" tag
            
        // benchmark List.sum
        let sw = Stopwatch()
        sw.Start()
        for i in 1..iter do
            mysum data |> ignore
        sw.Stop()
        let traw = (1000.0 * sw.Elapsed.TotalMilliseconds / float iter)
        printfn "raw: %.3fµs" traw

        if not (File.Exists logFile) then
            File.WriteAllLines(logFile, ["size;ag;raw"])

        File.AppendAllLines(logFile, [sprintf "%d;%f;%f" size tag traw])


    
        

            

