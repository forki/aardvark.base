#I @"..\..\bin\Debug"

#r "Mono.Reflection.dll"
#r "Aardvark.Base.dll"
#r "Aardvark.Base.Essentials.dll"
#r "Aardvark.Base.TypeProviders.dll"
#r "Aardvark.Base.FSharp.dll"

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Collections.Concurrent
open Mono.Reflection
open Aardvark.Base



module AgV8 =
    

    type ITraversal =
        abstract member Traverse : obj -> Option<obj>

    type ITraversal<'a> =
        inherit ITraversal
        abstract member Traverse : obj -> Option<'a>

    [<AbstractClass>]
    type AbstractTraversal<'a>() =
        abstract member Traverse : obj -> Option<'a>

        interface ITraversal with
            member x.Traverse o =
                match x.Traverse o with
                    | Some v -> Some (v :> obj)
                    | None -> None

        interface ITraversal<'a> with
            member x.Traverse o = x.Traverse o


    module private TraversalBuilder =

        let traversalAssembly = 
            AppDomain.CurrentDomain.DefineDynamicAssembly(
                AssemblyName("AgTraversals"), 
                AssemblyBuilderAccess.RunAndSave
            )

        let traversalModule =
            traversalAssembly.DefineDynamicModule(
                "MainModule",
                "AgTraversals.dll",
                true
            )

        // TODO: fill with all sem-functions
        let synMethods = Dictionary<string, MethodInfo[]>()
        let inhMethods = Dictionary<string, MethodInfo[]>()

        let traversalTypeCache = ConcurrentDictionary<string, Option<Type>>()

        


        let rec tryGetTraversal (name : string) : Option<Type> =
            traversalTypeCache.GetOrAdd(
                name,
                fun name ->
                    match synMethods.TryGetValue name with
                        | (true, mi) -> createTraversal name mi |> Some
                        | _ -> None
            )

        and rebuildFunction (name : string) (ldTarget : ILGenerator -> unit) (mi : MethodInfo) (bType : TypeBuilder) =
            
            let instructions = mi.GetInstructions()

            let parameters = mi.GetParameters() 

            let attributes =
                if mi.IsStatic then MethodAttributes.Public ||| MethodAttributes.Static
                else MethodAttributes.Public

            let meth = bType.DefineMethod(name, attributes, mi.ReturnType, parameters |> Array.map (fun pi -> pi.ParameterType))

            let il = meth.GetILGenerator()

            let jumpTargets =
                instructions
                    |> Seq.toList
                    |> List.collect (fun i -> 
                        match i.Operand with
                            | :? Instruction as i -> 
                                let label = il.DefineLabel()
                                [i, label]

                            | :? array<Instruction> as i ->
                                i |> Array.toList |> List.map (fun i -> i, il.DefineLabel())

                            | _ -> []
                    )
                    |> Dictionary.ofList

            let locals =
                instructions 
                    |> Seq.toList
                    |> List.choose (fun i ->
                        match i.Operand with
                            | :? LocalVariableInfo as v -> Some (v, il.DeclareLocal(v.LocalType))
                            | _ -> None
                    )
                    |> Dictionary.ofList

            let parameterOffset = 
                if mi.IsStatic then 0
                else 1

            for i in instructions do
                match jumpTargets.TryGetValue i with
                    | (true, l) -> il.MarkLabel(l)
                    | _ -> ()

                if i.OpCode = OpCodes.Ldarg_0 && not mi.IsStatic then
                    ldTarget il
                else
                    match i.Operand with
                        | null                          -> il.Emit(i.OpCode)
                        | :? int8 as o                  -> il.Emit(i.OpCode, o)
                        | :? uint8 as o                 -> il.Emit(i.OpCode, o)
                        | :? int16 as o                 -> il.Emit(i.OpCode, o)
                        | :? int as o                   -> il.Emit(i.OpCode, o)
                        | :? int64 as o                 -> il.Emit(i.OpCode, o)
                        | :? float as o                 -> il.Emit(i.OpCode, o)
                        | :? float32 as o               -> il.Emit(i.OpCode, o)
                        | :? string as o                -> il.Emit(i.OpCode, o)

                        | :? Type as t                  -> il.Emit(i.OpCode, t)
                        | :? MethodInfo as mi           -> il.EmitCall(i.OpCode, mi, null)
                        | :? FieldInfo as fi            -> il.Emit(i.OpCode, fi)
                        | :? ConstructorInfo as ci      -> il.Emit(i.OpCode, ci)
                        | :? SignatureHelper as sh      -> il.Emit(i.OpCode, sh)

                        | :? Instruction as t           -> il.Emit(i.OpCode, jumpTargets.[t])
                        | :? array<Instruction> as t    -> il.Emit(i.OpCode, t |> Array.map (fun ti -> jumpTargets.[ti]))

                        | :? LocalVariableInfo as l     -> il.Emit(i.OpCode, locals.[l])

                        | :? ThisParameter              -> ldTarget il
                        | :? ParameterInfo as p         -> il.Emit(i.OpCode, p.Position + parameterOffset)

                        | _                             -> failwithf "unsupported instruction: %A" i
                        

            meth

        and createTraversal (name : string) (methods : MethodInfo[]) =
            assert (methods |> Array.forall (fun mi -> mi.GetParameters().Length = 1))

            let retTypeCount =
                methods
                    |> Seq.map (fun mi -> mi.ReturnType)
                    |> PersistentHashSet.ofSeq
                    |> PersistentHashSet.count
                    
            let retType, baseType =
                if retTypeCount = 1 then 
                    let ret = methods.[0].ReturnType
                    ret, typedefof<AbstractTraversal<_>>.MakeGenericType [|ret|]
                else 
                    failwith "not implmented"

            let targetTypes =
                methods
                    |> Seq.filter (fun mi -> not mi.IsStatic)
                    |> Seq.map (fun mi -> mi, mi.DeclaringType)
                    |> HashMap.ofSeq

            let targetMap =
                targetTypes
                    |> HashMap.toSeq
                    |> Seq.map snd
                    |> PersistentHashSet.ofSeq
                    |> PersistentHashSet.toSeq
                    |> Seq.mapi (fun i t -> t, (i,Activator.CreateInstance(t)))
                    |> HashMap.ofSeq

            let tryGetTarget (mi : MethodInfo) =
                if mi.IsStatic then None
                else HashMap.tryFind mi.DeclaringType targetMap

            let traversalType = 
                traversalModule.DefineType(
                    name + "Traversal",
                    TypeAttributes.Class ||| TypeAttributes.Public,
                    baseType
                )

            let fTargets = traversalType.DefineField("m_targets", typeof<obj[]>, FieldAttributes.Private)

            let traverse = 
                traversalType.DefineMethod(
                    "Traverse",
                    MethodAttributes.Public ||| MethodAttributes.Virtual,
                    retType,
                    [|typeof<obj>|]
                )

            let il = traverse.GetILGenerator()










            failwith "sadsadas"



    type Impl =
        class
            val mutable public Offset : int

            member x.Sepp(a : int) = 
                Fun.Abs(a + x.Offset)

            new(o) = { Offset = o }
        end

    let testCreateStaticWrapper () =

        let mi = typeof<Impl>.GetMethod "Sepp"

        let traversalType = 
            TraversalBuilder.traversalModule.DefineType(
                "TestStaticWrapper",
                TypeAttributes.Class ||| TypeAttributes.Public,
                typeof<obj>
            )

        let t = traversalType.DefineField("target", typeof<Impl>, FieldAttributes.Private)

        let ldTarget(il : ILGenerator) =
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, t)

        let meth = traversalType |> TraversalBuilder.rebuildFunction "Sepp" ldTarget mi
            

        let ctor = traversalType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<Impl>|])

        let il = ctor.GetILGenerator()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Stfld, t)
        il.Emit(OpCodes.Ret)



        let t = traversalType.CreateType()
        let mi = t.GetMethod "Sepp"

        let instance = Activator.CreateInstance(t, Impl(1))

        mi.Invoke(instance, [|-10 :> obj|]) |> printfn "Sepp(1) = %A"
        



