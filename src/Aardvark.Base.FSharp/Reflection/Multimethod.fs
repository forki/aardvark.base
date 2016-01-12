#if COMPILED
namespace Aardvark.Base
#else
#I @"..\..\..\bin\Release\"
#r @"Aardvark.Base.dll"
#r @"Aardvark.Base.TypeProviders.dll"
#r @"Aardvark.Base.FSharp.dll"
open Aardvark.Base
#endif
open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading
open Microsoft.FSharp.Reflection

module private Helpers =
    
    type State = { assignment : HashMap<Type, Type> }
    type Result<'a> = { run : State -> Option<State * 'a> }

    type ResultBuilder() =
        member x.Bind(r : Result<'a>, f : 'a -> Result<'b>) =
            { run = fun s ->
                match r.run s with
                    | Some (s, v) ->
                        (f v).run s
                    | None ->
                        None
            }

        member x.Return(v : 'a) =
            { run = fun s -> Some(s,v) }

        member x.Zero() =
            { run = fun s -> Some(s, ()) }

        member x.Delay(f : unit -> Result<'a>) =
            { run = fun s -> f().run s }

        member x.Combine(l : Result<unit>, r : Result<'a>) =
            { run = fun s ->
                match l.run s with
                    | Some(s,()) -> r.run s
                    | None -> None
            }

        member x.For(seq : seq<'a>, f : 'a -> Result<unit>) =
            { run = fun s -> 
                let e = seq.GetEnumerator()

                let rec run(s) =
                    if e.MoveNext() then
                        match (f e.Current).run s with
                            | Some(s,()) ->
                                run s
                            | None ->
                                None
                    else
                        Some(s,())

                let res = run s
                e.Dispose()
                res
            }

        member x.While(guard : unit -> bool, body : Result<unit>) =
            { run = fun s ->
                if guard() then
                    match body.run s with
                        | Some (s,()) -> x.While(guard, body).run s
                        | None -> None
                else
                    Some(s,())
            }

    let result = ResultBuilder()

    let assign (t : Type) (o : Type) =
        { run = fun s ->
            match HashMap.tryFind t s.assignment with
                | Some(old) when old <> o ->
                    None
                | _ ->
                    Some({ s with assignment = HashMap.add t o s.assignment }, ())
        }

    let fail<'a> : Result<'a> = { run = fun s -> None }

    let success = { run = fun s -> Some(s, ()) }

    let rec tryInstantiateType (argType : Type) (realType : Type) =
        result {
            let genArg = if argType.IsGenericType then argType.GetGenericTypeDefinition() else argType

            if argType = realType then
                do! success
            elif argType.IsAssignableFrom realType then
                do! success

            elif argType.IsGenericParameter then
                do! assign argType realType

            elif argType.ContainsGenericParameters && realType.IsGenericType && realType.GetGenericTypeDefinition() = genArg then
                let argArgs = argType.GetGenericArguments()
                let realGen = realType.GetGenericTypeDefinition()
                let realArgs = realType.GetGenericArguments()

                for i in 0..realArgs.Length-1 do
                    let r = realArgs.[i]
                    let a = argArgs.[i]
                    do! tryInstantiateType a r

            elif argType.IsInterface then
                let iface = 
                    realType.GetInterfaces()
                        |> Array.tryFind (fun iface -> 
                            let gen = if iface.IsGenericType then iface.GetGenericTypeDefinition() else iface
                            gen = genArg
                        )

                match iface with
                    | Some iface ->
                        if iface.IsGenericType then
                            let ifaceArgs = iface.GetGenericArguments()
                            let argArgs = argType.GetGenericArguments()
                            for i in 0..ifaceArgs.Length - 1 do
                                do! tryInstantiateType argArgs.[i] ifaceArgs.[i]
                        else
                            ()
                    | None ->
                        do! fail

            else
                let baseType = realType.BaseType
                if isNull baseType then do! fail
                else do! tryInstantiateType argType baseType
        }

    let tryInstantiateMethodInfo (mi : MethodInfo) (real : Type[]) =
        result {
            let p = mi.GetParameters()
            if p.Length = real.Length then
                for i in 0..p.Length-1 do
                    do! tryInstantiateType (p.[i].ParameterType) real.[i]
            else
                do! fail
        }
            

    let tryInstantiate (mi : MethodInfo) (args : Type[]) =
        let parameters = mi.GetParameters()
        if parameters.Length = args.Length then
            if mi.IsGenericMethod then
                let mi = mi.GetGenericMethodDefinition()
                let m = tryInstantiateMethodInfo mi args
                match m.run { assignment = HashMap.empty } with
                    | Some (s,()) ->
                        let args = mi.GetGenericArguments() |> Array.map (fun a -> HashMap.find a s.assignment)
                        mi.MakeGenericMethod(args) |> Some

                    | None -> 
                        None
            else
                let works = Array.forall2 (fun (p : ParameterInfo) a -> p.ParameterType.IsAssignableFrom a) parameters args
                if works then Some mi
                else None
        else
            None

type IRuntimeMethod =
    [<Security.SuppressUnmanagedCodeSecurity>]
    abstract member Invoke : obj[] -> obj

module RuntimeMethodBuilder =
    open System.Reflection.Emit

    let internal bAss = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName("IRuntimeMethods"), AssemblyBuilderAccess.Run, null, Security.SecurityContextSource.CurrentAssembly)
    do bAss.PermissionSet.AddPermission(Security.Permissions.ReflectionPermission(Security.Permissions.PermissionState.Unrestricted)) |> ignore
    let internal bMod = bAss.DefineDynamicModule("MainModule")

    exception UnknownException of Type[]
    exception UnsupportedTypesException of Type[]


    let createTable (types : Type[]) =
        let handles = types |> Array.map (fun t -> t, t.TypeHandle.Value)

        let min = handles |> Seq.map snd |> Seq.min
        let max = handles |> Seq.map snd |> Seq.max
        let handleValues = handles |> Array.map (fun (_,h) -> (h - min))

        let rec tryCreate (size : nativeint) =
            let table = Array.zeroCreate (int size)
            let mutable success = true

            for t in types do
                let key = t.TypeHandle.Value % size
                if isNull table.[int key] then
                    table.[int key] <- t
                else
                    success <- false

            if success then 
                min, table
            else
                tryCreate (size + 1n)

        tryCreate (nativeint handles.Length)





    let inlineCache (argCount : int) (f : Type[] -> Option<obj * MethodInfo>) : obj[] -> obj =
        let created = ref false
        let current = ref Unchecked.defaultof<IRuntimeMethod>

        let all = List<Type[] * obj * MethodInfo>()

        let build() =
            let bType = bMod.DefineType(Guid.NewGuid().ToString(), TypeAttributes.Public ||| TypeAttributes.Class)
            let bMeth = bType.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<obj>, [|typeof<obj[]>|])

            let il = bMeth.GetILGenerator()

            let bTargets = bType.DefineField("targets", typeof<obj[]>, FieldAttributes.Private)

            let args = Array.init argCount (fun i -> il.DeclareLocal(typeof<obj>))
            let typeVars = Array.init argCount (fun i -> il.DeclareLocal(typeof<Type>))
            
            let arr = il.DeclareLocal(typeof<Type[]>)

            for i in 0..argCount-1 do
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Ldc_I4, i)
                il.Emit(OpCodes.Ldelem_Ref)

                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Stloc, args.[i])

                il.EmitCall(OpCodes.Callvirt, typeof<obj>.GetMethod "GetType", null)
                il.Emit(OpCodes.Stloc, typeVars.[i])

            for ai in 0..all.Count-1 do
                let (t, target, meth) = all.[ai]
                let paramters = meth.GetParameters()

                let fLabel = il.DefineLabel()

                for i in 0..argCount-1 do
                    let ti = t.[i]
                    il.Emit(OpCodes.Ldloc, typeVars.[i])
                    il.Emit(OpCodes.Ldtoken, ti)
                    il.Emit(OpCodes.Bne_Un, fLabel)

                // load the targets[ai]
                if not meth.IsStatic then
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldfld, bTargets)
                    il.Emit(OpCodes.Ldc_I4, ai)
                    il.Emit(OpCodes.Ldelem, meth.DeclaringType)

                        
                for i in 0..argCount-1 do
                    let at = paramters.[i].ParameterType
                    
                    il.Emit(OpCodes.Ldloc, args.[i])
    //                            il.Emit(OpCodes.Ldarg_1)
    //                            il.Emit(OpCodes.Ldc_I4, i)
    //                            il.Emit(OpCodes.Ldelem_Ref)
                    il.Emit(OpCodes.Unbox_Any, at)

                il.EmitCall(OpCodes.Call, meth, null)
                if meth.ReturnType = typeof<System.Void> then
                    il.Emit(OpCodes.Ldnull)
                elif meth.ReturnType.IsValueType then
                    il.Emit(OpCodes.Box, meth.ReturnType)
                else
                    il.Emit(OpCodes.Unbox_Any, typeof<obj>)

                il.Emit(OpCodes.Ret)

                il.MarkLabel(fLabel)


            il.Emit(OpCodes.Ldc_I4, argCount)
            il.Emit(OpCodes.Newarr, typeof<Type>)
            il.Emit(OpCodes.Stloc, arr)

            for i in 0..argCount-1 do
                il.Emit(OpCodes.Ldloc, arr)
                il.Emit(OpCodes.Ldc_I4, i)
                il.Emit(OpCodes.Ldloc, typeVars.[i])
                il.Emit(OpCodes.Stelem_Ref)

            
            il.Emit(OpCodes.Ldloc, arr)
            il.Emit(OpCodes.Newobj, typeof<UnknownException>.GetConstructor [|typeof<Type[]>|])
            il.Emit(OpCodes.Throw)
            il.Emit(OpCodes.Ret)


            let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<obj[]>|])
            let il = bCtor.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, bTargets)
            il.Emit(OpCodes.Ret)

            bType.AddInterfaceImplementation(typeof<IRuntimeMethod>)
            bType.DefineMethodOverride(bMeth, typeof<IRuntimeMethod>.GetMethod "Invoke")

            let t = bType.CreateType()

            let ctor = t.GetConstructor [|typeof<obj[]> |]
            let targets = all |> Seq.map (fun (_,t,_) -> t) |> Seq.toArray
            ctor.Invoke([|targets :> obj|]) |> unbox<IRuntimeMethod>        

        let buildHash() =
            if argCount <> 1 then failwith "not implemented"

            let cases = all |> Seq.map (fun (t,_,_) -> t.[0]) |> Seq.toArray
            let caseIds = cases |> Array.mapi (fun i t -> t,i) |> Dictionary.ofArray

            let bType = bMod.DefineType(Guid.NewGuid().ToString(), TypeAttributes.Public ||| TypeAttributes.Class)
            let bIds = bType.DefineField("caseIds", typeof<Dictionary<Type, int>>, FieldAttributes.Private)
            let bTargets = bType.DefineField("targets", typeof<obj[]>, FieldAttributes.Private)

            let bMeth = bType.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<obj>, [|typeof<obj[]>|])
            let il = bMeth.GetILGenerator()
            let id = il.DeclareLocal(typeof<int>)
            let arg = il.DeclareLocal(typeof<obj>)
            let argType = il.DeclareLocal(typeof<Type>)
            let arr = il.DeclareLocal(typeof<Type[]>)

            let caseLabels = Array.init cases.Length (fun i -> il.DefineLabel())
            let notFoundLabel = il.DefineLabel()

            // load the id-dictionary
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, bIds)

            // get the argument-type
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ldelem_Ref)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Stloc, arg)

            il.EmitCall(OpCodes.Callvirt, typeof<obj>.GetMethod "GetType", null)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Stloc, argType)
            
            // Type hashing using a perfect-match table
            (*
            let min = 0n
            let table = [||]
            let bTable : FieldBuilder = failwith ""
            
            il.Emit(OpCodes.Ldfld, bTable)

            il.EmitCall(OpCodes.Call, typeof<Type>.GetProperty("TypeHandle").GetMethod, null)
            il.EmitCall(OpCodes.Call, typeof<RuntimeTypeHandle>.GetProperty("Value").GetMethod, null)
            
            il.Emit(OpCodes.Ldc_I8, int64 min)
            il.Emit(OpCodes.Sub)

            il.Emit(OpCodes.Ldc_I8, table)
            il.Emit(OpCodes.Rem)
            
            il.Emit(OpCodes.Ldelem, typeof<int>)
            *)


            // load &id
            il.Emit(OpCodes.Ldloca, id)

            // call dict.TryGetValue(argType, &id)
            il.EmitCall(OpCodes.Call, typeof<Dictionary<Type, int>>.GetMethod("TryGetValue"), null)
            il.Emit(OpCodes.Brfalse_S, notFoundLabel)

            // switch(id)
            il.Emit(OpCodes.Ldloc, id)
            il.Emit(OpCodes.Switch, caseLabels)
            il.Emit(OpCodes.Br_S, notFoundLabel)


            for ci in 0..all.Count-1 do
                let (_,target,meth) = all.[ci]

                il.MarkLabel(caseLabels.[ci])

                // perform the actual call
                if not meth.IsStatic then
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldfld, bTargets)
                    il.Emit(OpCodes.Ldc_I4, ci)
                    il.Emit(OpCodes.Ldelem, meth.DeclaringType)
                      
                let at = meth.GetParameters().[0].ParameterType
                il.Emit(OpCodes.Ldloc, arg)
                il.Emit(OpCodes.Unbox_Any, at)

                il.EmitCall(OpCodes.Call, meth, null)
                if meth.ReturnType = typeof<System.Void> then
                    il.Emit(OpCodes.Ldnull)
                elif meth.ReturnType.IsValueType then
                    il.Emit(OpCodes.Box, meth.ReturnType)
                else
                    il.Emit(OpCodes.Unbox_Any, typeof<obj>)

                il.Emit(OpCodes.Ret)


            il.MarkLabel(notFoundLabel)


            il.Emit(OpCodes.Ldc_I4, argCount)
            il.Emit(OpCodes.Newarr, typeof<Type>)

            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ldloc, argType)
            il.Emit(OpCodes.Stelem_Ref)


            il.Emit(OpCodes.Newobj, typeof<UnknownException>.GetConstructor [|typeof<Type[]>|])
            il.Emit(OpCodes.Throw)
            il.Emit(OpCodes.Ret)


            let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<Dictionary<Type, int>>; typeof<obj[]>|])
            let il = bCtor.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
            
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, bIds)
            
            
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Stfld, bTargets)
            il.Emit(OpCodes.Ret)

            bType.AddInterfaceImplementation(typeof<IRuntimeMethod>)
            bType.DefineMethodOverride(bMeth, typeof<IRuntimeMethod>.GetMethod "Invoke")

            let t = bType.CreateType()

            let ctor = t.GetConstructor [|typeof<Dictionary<Type, int>>; typeof<obj[]> |]
            let targets = all |> Seq.map (fun (_,t,_) -> t) |> Seq.toArray
            ctor.Invoke([|caseIds :> obj; targets :> obj|]) |> unbox<IRuntimeMethod>        

        
        let buildTable() =
            if argCount <> 1 then failwith "not implemented"

            let methods = all |> Seq.mapi (fun i (t,tar,m) -> t.[0],(tar, m, i)) |> Dictionary.ofSeq
            let cases = all |> Seq.map (fun (t,_,_) -> t.[0]) |> Seq.toArray

            let bType = bMod.DefineType(Guid.NewGuid().ToString(), TypeAttributes.Public ||| TypeAttributes.Class)
            let bTargets = bType.DefineField("targets", typeof<obj[]>, FieldAttributes.Private)

            let bMeth = bType.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<obj>, [|typeof<obj[]>|])
            let il = bMeth.GetILGenerator()
            //let id = il.DeclareLocal(typeof<int>)
            let arg = il.DeclareLocal(typeof<obj>)
            let argType = il.DeclareLocal(typeof<Type>)
            //let arr = il.DeclareLocal(typeof<Type[]>)

            //let caseLabels = Array.init cases.Length (fun i -> il.DefineLabel())
            let notFoundLabel = il.DefineLabel()

            let (tMin, typeTable) = createTable cases

            let caseLabels = 
                typeTable |> Array.map (fun t ->
                    il.DefineLabel()
                )


            // get the argument-type
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ldelem_Ref)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Stloc, arg)

            il.EmitCall(OpCodes.Callvirt, typeof<obj>.GetMethod "GetType", null)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Stloc, argType)
            

            // Type hashing using a perfect-match table
            il.EmitCall(OpCodes.Call, typeof<Type>.GetProperty("TypeHandle").GetMethod, null)
            il.EmitCall(OpCodes.Call, typeof<RuntimeTypeHandle>.GetProperty("Value").GetMethod, null)
            

            il.Emit(OpCodes.Pop)
            il.Emit(OpCodes.Ldc_I4, 0)

            if sizeof<nativeint> = 8 then il.Emit(OpCodes.Conv_I8)
            else il.Emit(OpCodes.Conv_I4)
             
            if sizeof<nativeint> = 8 then il.Emit(OpCodes.Ldc_I8, int64 tMin)
            else il.Emit(OpCodes.Ldc_I4, int32 tMin)
            il.Emit(OpCodes.Sub)

            if sizeof<nativeint> = 8 then il.Emit(OpCodes.Ldc_I8, int64 caseLabels.Length)
            else il.Emit(OpCodes.Ldc_I4, caseLabels.Length)
            il.Emit(OpCodes.Rem)

            // switch(id)
            il.Emit(OpCodes.Conv_I4)
            il.Emit(OpCodes.Switch, caseLabels)
            il.Emit(OpCodes.Br_S, notFoundLabel)


            for ci in 0..typeTable.Length-1 do
                let t = typeTable.[ci]
                if not (isNull t) then
                    let (target,meth,targetIndex) = methods.[t]

                    il.MarkLabel(caseLabels.[ci])

                    // perform the actual call
                    if not meth.IsStatic then
                        il.Emit(OpCodes.Ldarg_0)
                        il.Emit(OpCodes.Ldfld, bTargets)
                        il.Emit(OpCodes.Ldc_I4, targetIndex)
                        il.Emit(OpCodes.Ldelem, meth.DeclaringType)
                      
                    let at = meth.GetParameters().[0].ParameterType
                    il.Emit(OpCodes.Ldloc, arg)
                    il.Emit(OpCodes.Unbox_Any, at)

                    il.EmitCall(OpCodes.Call, meth, null)
                    if meth.ReturnType = typeof<System.Void> then
                        il.Emit(OpCodes.Ldnull)
                    elif meth.ReturnType.IsValueType then
                        il.Emit(OpCodes.Box, meth.ReturnType)
                    else
                        il.Emit(OpCodes.Unbox_Any, typeof<obj>)

                    il.Emit(OpCodes.Ret)

                else
                    il.MarkLabel(caseLabels.[ci])
                    il.Emit(OpCodes.Br_S, notFoundLabel)

            il.MarkLabel(notFoundLabel)


            il.Emit(OpCodes.Ldc_I4, argCount)
            il.Emit(OpCodes.Newarr, typeof<Type>)

            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ldloc, argType)
            il.Emit(OpCodes.Stelem_Ref)


            il.Emit(OpCodes.Newobj, typeof<UnknownException>.GetConstructor [|typeof<Type[]>|])
            il.Emit(OpCodes.Throw)
            il.Emit(OpCodes.Ret)


            let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<obj[]>|])
            let il = bCtor.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
            
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, bTargets)
            il.Emit(OpCodes.Ret)

            bType.AddInterfaceImplementation(typeof<IRuntimeMethod>)
            bType.DefineMethodOverride(bMeth, typeof<IRuntimeMethod>.GetMethod "Invoke")

            let t = bType.CreateType()

            let ctor = t.GetConstructor [|typeof<obj[]> |]
            let targets = all |> Seq.map (fun (_,t,_) -> t) |> Seq.toArray
            ctor.Invoke([|targets :> obj|]) |> unbox<IRuntimeMethod>       


        let rebuild(newArgTypes : Type[]) =
            created := true
            let meth = f newArgTypes
            match meth with
                | Some (target, meth) ->
                    all.Add(newArgTypes, target, meth)
                    if all.Count > 1 then buildTable()
                    else build()
                | _ ->
                    raise <| UnsupportedTypesException(newArgTypes)

        let rec invoke(args) =
            if !created then
                try current.Value.Invoke args
                with UnknownException arr -> 
                    current := rebuild arr
                    current.Value.Invoke(args)
            else
                current := rebuild (args |> Array.map (fun a -> a.GetType()))
                current.Value.Invoke(args)

        invoke

exception MultimethodException              
type Multimethod(parameterCount : int, initial : seq<MethodInfo * obj>)  =
    let implementations = Dictionary.ofSeq (initial)
    let mutable initialized = false

    static let instances = Dictionary<Type, obj>()

    static let createInstance (t : Type) =
        lock instances (fun () ->
            match instances.TryGetValue t with
                | (true, i) -> i
                | _ ->
                    let i = Activator.CreateInstance t
                    instances.[t] <- i
                    i
        )

    static let createTarget (mi : MethodInfo) =
        if mi.IsStatic then null
        else createInstance mi.DeclaringType

    let tryGetMethodInfo(retType : Type) (types : Type[])=
        let goodOnes = 
            implementations.Keys
                |> Seq.choose (fun mi -> Helpers.tryInstantiate mi types)
                |> Seq.filter (fun mi -> retType.IsAssignableFrom mi.ReturnType)
                |> Seq.cast
                |> Seq.toArray

        if goodOnes.Length > 0 then
            let selected = 
                Type.DefaultBinder.SelectMethod(
                    BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.InvokeMethod,
                    goodOnes,
                    types,
                    types |> Array.map (fun _ -> ParameterModifier())
                ) |> unbox<MethodInfo>
                
            Some (selected, implementations.[selected])
        else
            None

    let methodCache = 
        RuntimeMethodBuilder.inlineCache parameterCount (fun types -> 
            initialized <- true
            match tryGetMethodInfo typeof<obj> types with
                | Some(mi,target) -> Some(target, mi)
                | _ -> None
        )


    static member GetTarget (mi : MethodInfo) = createTarget mi 

    member x.Add(mi : MethodInfo) =
        if initialized then
            failwith "cannot modify Multimethod after first call"

        if mi.GetParameters().Length <> parameterCount then
            failwithf "cannot add %A to Multimethod due to parameter-count mismatch" mi

        if mi.IsGenericMethod then
            let mi = mi.GetGenericMethodDefinition()
            implementations.Add(mi, createTarget mi) |> ignore
        else
            implementations.Add(mi, createTarget mi) |> ignore

    member x.Add(target : obj, mi : MethodInfo) =
        if initialized then
            failwith "cannot modify Multimethod after first call"

        if mi.GetParameters().Length <> parameterCount then
            failwithf "cannot add %A to Multimethod due to parameter-count mismatch" mi

        if mi.IsGenericMethod then
            let mi = mi.GetGenericMethodDefinition()
            implementations.Add(mi, target) |> ignore
        else
            implementations.Add(mi, target) |> ignore


    member x.TryInvoke(args : obj[], [<Out>] result : byref<obj>) : bool =
        try
            let res = methodCache args
            result <- res
            true

        with _ ->
            false

    member x.InvokeSafe(args : obj[]) =
        if args.Length <> parameterCount then
            raise <| MultimethodException
        else
            try
                methodCache args
            with :? RuntimeMethodBuilder.UnsupportedTypesException ->
                raise <| MultimethodException

    member x.Invoke(args : obj[]) =
        methodCache args

    new(parameterCount : int) = 
        Multimethod(parameterCount, Seq.empty)

    new(methods : seq<MethodInfo>) = 
        let count = (methods |> Seq.head).GetParameters().Length
        Multimethod(count, methods |> Seq.map (fun mi -> mi, createTarget mi))

module MultimethodTest =

    type IA<'a> =
        abstract member V : int
        abstract member Sepp : unit -> int

    type A(v : int) = 
        member x.V = v
        member x.Sepp() = v + v
        interface IA<float32> with
            member x.V = v
            member x.Sepp() = x.Sepp()

    type B(a : int) =
        inherit A(a)
        member x.A = a

    type C(v : int) =
        interface IA<obj> with
            member x.V = v
            member x.Sepp() = v

    type D(v : int) =
        inherit B(v)

    type Sepp() =
        member x.Value(ia : IA<'a>) =
            printfn "IA<%A>" typeof<'a>
            ia.V

        member x.Value(a : A) =
            printfn "A"
            a.V

        member x.Value(b : B) =
            b.A

    open System.Diagnostics


    let run() =
        let m = Multimethod(typeof<Sepp>.GetMethods() |> Seq.filter (fun mi -> mi.Name = "Value"))


//        let invoke (o : obj) =
//            match m.TryInvoke([|o|]) with 
//                | (true, res) -> printfn "value(%A) = %A" o res
//                | _ -> () 

        let ours(iter : int) =

            let args = [|B 10 :> obj|]
            let sw = Stopwatch()
            let mutable res = Unchecked.defaultof<obj>
            for i in 0..1000 do
                m.TryInvoke(args, &res) |> ignore

            let args = [|D 10 :> obj|]
            let sw = Stopwatch()
            let mutable res = Unchecked.defaultof<obj>
            for i in 0..1000 do
                m.TryInvoke(args, &res) |> ignore

            sw.Start()
            for i in 1..iter do
                m.TryInvoke(args, &res) |> ignore
            sw.Stop()
            let t = (1000000.0 * sw.Elapsed.TotalMilliseconds / float iter)
            printfn "ours: %.3fns" t
            t

        let sepp(iter : int) =
            let s = Sepp()
            let args = D 10 :> IA<float32>
            let sw = Stopwatch()

            for i in 0..1000 do
                args.Sepp() |> ignore

            sw.Start()
            for i in 1..iter do
                args.Sepp() |> ignore
            sw.Stop()
            let t = (1000000.0 * sw.Elapsed.TotalMilliseconds / float iter)
            printfn "sepp: %.3fns" t
            t

        let nop() =
            let s = Sepp()
            let args = D 10 :> IA<float32>
            let sw = Stopwatch()
            let mutable iter = 0
            let mutable res = Unchecked.defaultof<obj>

            let mutable total = 0
            sw.Start()
            while sw.Elapsed.TotalMilliseconds < 1000.0 do 
                iter <- iter + 1
                total <- total + 1
            sw.Stop()
            let t = (1000000.0 * sw.Elapsed.TotalMilliseconds / float iter)
            printfn "nop:  %.3fns" t
            iter

        let iter = nop()
        let tsepp = sepp(4 * iter)
        let tours = ours(4 * iter)
        printfn "factor: %.2f" (tours / tsepp)

