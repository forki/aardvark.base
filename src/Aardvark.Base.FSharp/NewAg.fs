namespace Aardvark.Base

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading
open Microsoft.FSharp.Reflection


module NewAg =
    type IAgMethod =
        abstract member Invoke : string -> obj -> Option<obj>

    type IObjFun =
        abstract member Invoke : obj -> obj

    let private inhFunctions = Dictionary<string, Multimethod>()
    let private synFunctions = Dictionary<string, Multimethod>()
    let private rootFunctions = Dictionary<string, Multimethod>()
    

    type Scope =
        {
            parent : Scope
            sourceNode : obj
            inhValues : Dictionary<string, Option<obj>>
        }

    let private noScope = { parent = Unchecked.defaultof<_>; sourceNode = null; inhValues = null }
    let private currentScope = new ThreadLocal<ref<Scope>>(fun () -> ref noScope)
    let private tempStore = new ThreadLocal<Dictionary<obj * string, obj>>(fun () -> Dictionary.empty)

    module Delay =

        open System
        open System.Reflection
        open System.Collections.Generic
        open Microsoft.FSharp.Reflection
        open System.Reflection.Emit

        type FSharpFuncConst<'a>(value) =
            inherit FSharpFunc<unit, 'a>()

            override x.Invoke(u : unit) =
                value

        let ctorCache = Dictionary<Type, ConstructorInfo>()
        let creatorCache = Dictionary<Type, IObjFun>()

        let getCtor (fType : Type) =
            lock ctorCache (fun () ->
                match ctorCache.TryGetValue fType with
                    | (true, ctor) -> ctor
                    | _ ->
                        let (ta, tr) = FSharpType.GetFunctionElements fType
                        if ta <> typeof<unit> then 
                            failwithf "unexpected arg-type: %A" ta
                        let t = typedefof<FSharpFuncConst<_>>.MakeGenericType [|tr|]
                        let ctor = t.GetConstructor [|tr|]
                        ctorCache.[fType] <- ctor
                        ctor
            )

        let getCreator (fType : Type) : IObjFun =
            lock creatorCache (fun () ->
                match creatorCache.TryGetValue fType with
                    | (true, c) -> c
                    | _ -> 
                        let ctor = getCtor fType
                        let (_, tr) = FSharpType.GetFunctionElements fType
                        let bType = RuntimeMethodBuilder.bMod.DefineType(Guid.NewGuid().ToString())
            
                        let bInvoke = bType.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<obj>, [|typeof<obj>|])
                        let il = bInvoke.GetILGenerator()
                        il.Emit(OpCodes.Ldarg_1)
                        il.Emit(OpCodes.Unbox_Any, tr)
                        il.Emit(OpCodes.Newobj, ctor)
                        il.Emit(OpCodes.Unbox_Any, typeof<obj>)
                        il.Emit(OpCodes.Ret)


                        let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
                        let il = bCtor.GetILGenerator()

                        il.Emit(OpCodes.Ldarg_0)
                        il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
                        il.Emit(OpCodes.Ret)

                        bType.AddInterfaceImplementation(typeof<IObjFun>)
                        bType.DefineMethodOverride(bInvoke, typeof<IObjFun>.GetMethod "Invoke")


                        let t = bType.CreateType()
                        let ctor = t.GetConstructor [||]

                        let c = ctor.Invoke [||] |> unbox<IObjFun>
                        creatorCache.[fType] <- c
                        c
            )


        type private DelayImpl<'a>() =
            static let ctor = getCreator typeof<'a>
            static member Create(o : obj) = ctor.Invoke o |> unbox<'a>

        let delay (value : obj) : 'a =
            DelayImpl<'a>.Create value

    module private RootFunctions =
        open System.Reflection.Emit


        let contentType (t : Type) =
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Ag.Root<_>> then
                t.GetGenericArguments().[0]
            else
                t

        let wrap (target : obj, mi : MethodInfo) =
            let parameters = mi.GetParameters()
            let parameterTypes = parameters |> Array.map (fun p -> contentType p.ParameterType)
            let bType = RuntimeMethodBuilder.bMod.DefineType(Guid.NewGuid().ToString())

            let bTarget = 
                if mi.IsStatic then null
                else bType.DefineField("_target", mi.DeclaringType, FieldAttributes.Private)

            let bMeth = bType.DefineMethod("Invoke", MethodAttributes.Public, mi.ReturnType, parameterTypes)
            let il = bMeth.GetILGenerator()

            il.Emit(OpCodes.Nop)
            if not mi.IsStatic then
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldfld, bTarget)
            

            for i in 0..parameters.Length - 1 do
                let p = parameters.[i]
                il.Emit(OpCodes.Ldarg, i + 1)
                il.Emit(OpCodes.Unbox_Any, typeof<obj>)
                il.Emit(OpCodes.Newobj, typedefof<Ag.Root<_>>.MakeGenericType(parameterTypes.[i]).GetConstructor [|typeof<obj>|])



            il.EmitCall(OpCodes.Call, mi, null)
            il.Emit(OpCodes.Ret)


            if mi.IsStatic then
                let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
                let il = bCtor.GetILGenerator()
                il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
                il.Emit(OpCodes.Ret)
            else
                let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|mi.DeclaringType|])
                let il = bCtor.GetILGenerator()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Stfld, bTarget)
                il.Emit(OpCodes.Ret)


            let t = bType.CreateType()

            if mi.IsStatic then
                let ctor = t.GetConstructor [||]
                ctor.Invoke [||], t.GetMethod "Invoke"
            else
                let ctor = t.GetConstructor [|mi.DeclaringType|]
                ctor.Invoke [|target|], t.GetMethod "Invoke"

    module private AgFunction =
        open System.Reflection.Emit

        let createSyn(inner : Dictionary<string, Multimethod>) =
            let inner = Dictionary.toArray inner
            let bType = RuntimeMethodBuilder.bMod.DefineType("SynTraversal")

            let bInner = bType.DefineField("inner", typeof<Multimethod[]>, FieldAttributes.Private)

            let bInvoke = bType.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<Option<obj>>, [|typeof<string>; typeof<obj> |])
            let il = bInvoke.GetILGenerator()
        
            for i in 0..inner.Length-1 do
                let (name,target) = inner.[i]
                let fLabel = il.DefineLabel()

                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Ldstr, name)
                il.EmitCall(OpCodes.Call, typeof<string>.GetMethod("Equals", [|typeof<string>; typeof<string>|]), null)
                il.Emit(OpCodes.Brfalse, fLabel)
                
                
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldfld, bInner)
                il.Emit(OpCodes.Ldc_I4, i)
                il.Emit(OpCodes.Ldelem_Ref)
                
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Newarr, typeof<obj>)
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ldarg_2)
                il.Emit(OpCodes.Stelem_Ref)

                
                il.EmitCall(OpCodes.Call, typeof<Multimethod>.GetMethod "Invoke", null)
                il.Emit(OpCodes.Newobj, typeof<Option<obj>>.GetConstructor [|typeof<obj>|])
                il.Emit(OpCodes.Ret)
                il.MarkLabel(fLabel)

            il.Emit(OpCodes.Ldnull)
            il.Emit(OpCodes.Unbox_Any, typeof<Option<obj>>)
            il.Emit(OpCodes.Ret)

            let bCtor = bType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<Multimethod[]>|])
            let il = bCtor.GetILGenerator()

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, bInner)
            il.Emit(OpCodes.Ret)


            bType.AddInterfaceImplementation(typeof<IAgMethod>)
            bType.DefineMethodOverride(bInvoke, typeof<IAgMethod>.GetMethod "Invoke")


            let t = bType.CreateType()

            let ctor = t.GetConstructor [|typeof<Multimethod[]>|]
            let instance = ctor.Invoke [|Array.map snd inner|] |> unbox<IAgMethod>
            instance

    let private synMeth = lazy ( AgFunction.createSyn synFunctions )

    let init() =
        let types = Introspection.GetAllTypesWithAttribute<Ag.Semantic>()

        for t in types do
            let methods = t.E0.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)

            for m in methods do
                let parameters = m.GetParameters()
                if parameters.Length = 1 then
                    let valid = 
                        if m.IsGenericMethod then
                            let free = HashSet(m.GetGenericArguments())
                            let bound = HashSet(m.GetParameters() |> Seq.collect (fun p -> p.ParameterType.GetGenericArguments()))

                            if free.SetEquals bound then
                                true
                            else
                                false
                        else
                            true
                    if not valid then
                        Log.warn "found generic semantic function %A whose parameters are not bound by the arguments" m
                    else 
                        let name = m.Name
                        let parameterType = parameters.[0].ParameterType

                        if parameterType.IsGenericType && parameterType.GetGenericTypeDefinition() = typedefof<Ag.Root<_>> then
                            let mm = 
                                match rootFunctions.TryGetValue name with
                                    | (true, mm) -> mm
                                    | _ ->
                                        let mm = Multimethod(1)
                                        rootFunctions.[name] <- mm
                                        mm
                            let (t,m) = RootFunctions.wrap (Multimethod.GetTarget m,m)
                            mm.Add(t, m)


                        elif m.ReturnType = typeof<unit> || m.ReturnType = typeof<System.Void> then
                            match inhFunctions.TryGetValue name with
                                | (true, mm) -> 
                                    mm.Add m
                                | _ -> 
                                    let mm = Multimethod [m]
                                    inhFunctions.[name] <- mm

                        else
                            match synFunctions.TryGetValue name with
                                | (true, mm) -> 
                                    mm.Add m
                                | _ -> 
                                    let mm = Multimethod [m]
                                    synFunctions.[name] <- mm



    let getCurrentScope() : Scope =
        !currentScope.Value

    let inline private useScope (s : Scope) (f : unit -> 'a) =
        let r = currentScope.Value
        let old = !r
        r := s
        try f()
        finally r := old


    let rec private tryInheritInternal (mm : Multimethod) (scopeRef : ref<Scope>) (name : string) =
        let scope = getCurrentScope()

        match scope.inhValues.TryGetValue name with
            | (true, v) -> v
            | _ ->
                if isNull scope.parent.sourceNode then
                    match rootFunctions.TryGetValue(name) with
                        | (true, mm) ->
                            match mm.TryInvoke([|scope.sourceNode|]) with
                                | (true, _) ->
                                    let ts = tempStore.Value
                                    let res =
                                        match ts.TryGetValue((scope.sourceNode, name)) with
                                            | (true, v) -> Some v
                                            | _ -> 
                                                failwithf "[Ag] invalid root method for attribute %s which does not write to %A" name scope.sourceNode
                            
                                    ts.Clear()
                                    res
                                | _ ->
                                    None
                        | _ ->
                            None
                else
                    scopeRef := scope.parent
                    let res =
                        match mm.TryInvoke([|scope.parent.sourceNode|]) with
                            | (true, _) -> 
                                let ts = tempStore.Value
                                let res =
                                    match ts.TryGetValue((scope.sourceNode, name)) with
                                        | (true, v) -> Some v
                                        | _ ->
                                            match ts.TryGetValue((Ag.anyObject, name)) with
                                                | (true, v) -> Some v
                                                | _ ->  
                                                    failwithf "[Ag] invalid inherit method for attribute %s which does not write to %A" name scope.sourceNode
                            
                                ts.Clear()
                                res

                            | _ -> tryInheritInternal mm scopeRef name
                            

                    scope.inhValues.[name] <- res
                    res
     
    let tryInherit (name : string) (o : obj) =
        let ref = currentScope.Value
        if isNull ref.Value.sourceNode then
            None
        else
            if not (Unchecked.equals ref.Value.sourceNode o) then
                failwithf "inheriting %s for %A but current scope induces %A" name o ref.Value.sourceNode

            match inhFunctions.TryGetValue name with
                | (true, mm) ->
                    tryInheritInternal mm ref name
                | _ ->
                    None


    let inline private trySynthesize (name : string) (o : obj) =
        useScope { parent = getCurrentScope(); sourceNode = o; inhValues = Dictionary.empty } (fun () ->
            try
                synMeth.Value.Invoke name o
            with :? MultimethodException | :? RuntimeMethodBuilder.UnsupportedTypesException ->
                None
        )
          
    let tryGetAttributeValue (name : string) (o : obj) =
        match trySynthesize name o with
            | Some v -> Some v
            | None -> tryInherit name o

    let (?<-) (n : obj) (name : string) (value : 'a) =
        tempStore.Value.[(n, name)] <- value
        

    type private AttImpl<'a>() =
        static let isSyn = typeof<'a>.Name.StartsWith "FSharpFunc" 
        static let f =
            if isSyn then
                fun (n : obj) (name : string) ->
                    match trySynthesize name n with
                        | Some v -> 
                            Delay.delay v
                        | None -> 
                            failwithf "[Ag] unable to find synthesized attribute %s for node type: %A" name (n.GetType())
            else
                fun (n : obj) (name : string) ->
                    match n with
                        | :? Scope as s ->
                            match useScope s (fun () -> tryInherit name s.sourceNode) with
                                | Some (:? 'a as v) -> v
                                | Some v ->
                                    failwithf "[Ag] unexpected type for inherited attribute %s on node type %A: %A" name (n.GetType()) (v.GetType())
                                | _ -> 
                                    failwithf "[Ag] unable to find inherited attribute %s on node type: %A" name (s.sourceNode.GetType())
                        | _ ->
                            match tryInherit name n with
                                | Some (:? 'a as v) -> v
                                | Some v ->
                                    failwithf "[Ag] unexpected type for inherited attribute %s on node type %A: %A" name (n.GetType()) (v.GetType())
                                | None -> 
                                    failwithf "[Ag] unable to find inherited attribute %s on node type: %A" name (n.GetType())


        static member Invoke (n : obj, name : string) : 'a = f n name

    let (?) (n : obj) (name : string) : 'a =
        AttImpl<'a>.Invoke(n, name)
                    

module NewAgTest =
    open Ag
    open NewAg
    open System.Diagnostics

    type INode = interface end

    type Env(a : INode) =
        interface INode
        member x.C = a

    type Leaf(a : int) =
        let mutable a = a
        interface INode
        member x.A
            with get() = a
            and set v = a <- v
        
    type Node(c : INode) =
        interface INode
        member x.C = c 

    [<Ag.Semantic>]
    type Sem() =
        member x.Inh(r : Ag.Root<INode>) =
            r.Child?Inh <- 0
            
        member x.Inh(n : Node) =
            n.AllChildren?Inh <- 1 + n?Inh

        member x.Value(a : Env) : int =
            a.C?Value()

        member x.Value(a : Leaf) =
            a.A + a?Inh

        member x.Value(n : Node) : int =
            n.C?Value()
        


    type IList =
        abstract member Sum : unit -> int

    type Cons(head : int, tail : IList) =
        interface IList with
            member x.Sum() = head + tail.Sum()
        member x.Head = head
        member x.Tail = tail

        

    type Nil() =
        interface IList with
            member x.Sum() = 0

    [<Ag.Semantic>]
    type ListSem() =
        member x.Sum(l : Cons) =
            l.Head + l.Tail?Sum()
            
        member x.Sum(n : Nil) =
            0 

    let rec directSum (l : IList) =
        l.Sum()

    let run() =
        NewAg.init()

        let rec longList (l : int) =
            if l <= 0 then Nil() :> IList
            else Cons(1, longList (l-1)) :> IList

        let size = 100
        let l = longList size

        let nop() =
            let sw = Stopwatch()
            let mutable iter = 0
            let mutable res = Unchecked.defaultof<obj>

            let mutable total = 0
            sw.Start()
            while sw.Elapsed.TotalMilliseconds < 1000.0 do 
                total <- total + directSum l
                iter <- iter + 1
            sw.Stop()
            printfn "iter:  %d" iter
            iter

        let direct(iter : int) =
            let sw = Stopwatch()
            let mutable total = 0
            sw.Start()
            for i in 1..iter do
                total <- total + directSum l
            sw.Stop()
            printfn "direct: %.3fns (%A)" (1000000.0 * sw.Elapsed.TotalMilliseconds / float (iter * size)) (total /iter)


        let ag(iter : int) =
            let sw = Stopwatch()
            let mutable total = 0
            sw.Start()
            for i in 1..iter do
                total <- total + l?Sum()
            sw.Stop()
            printfn "ag:     %.3fns (%A)" (1000000.0 * sw.Elapsed.TotalMilliseconds / float (iter * size)) (total /iter)

        let agProfile() =
            let mutable total = 0
            while true do
                total <- total + l?Sum()




        let directv = directSum l
        printfn "direct: %A" directv
        let agv : int = l?Sum()
        printfn "ag:     %A" agv

        let iter = nop()
        direct(iter)
        ag(iter / 20)
        

        printfn "running ag for profiling"
        agProfile()
        Environment.Exit 0


        let l = Leaf 1
        let t = Node(Node(l))
        let v : int = t?Value()
        printfn "%A" v


        l.A <- 5
        let v : int = t?Value()
        printfn "%A" v

        NewAg.tryGetAttributeValue "Sepp" v |> printfn "Sepp = %A"



