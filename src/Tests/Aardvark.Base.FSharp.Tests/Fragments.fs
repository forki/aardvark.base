﻿namespace Aardvark.Base.FSharp.Tests
#nowarn "44"

open System
open Aardvark.Base
open FsUnit
open NUnit.Framework
open System.Runtime.InteropServices
open System.Diagnostics
open System.Collections.Generic

module FragmentTests =

    type MyDelegate = delegate of int -> unit

    let calls = List<obj[]>()

    let func(a : int) =
        printfn "asdasdasdas"
        //calls.Add([|a :> obj; b; c; d; e|])
        //sprintf "{ a = %d; b = %d; c = %d; d = %d; e = %d }" a b c d e |> Console.WriteLine

    let myfun = MyDelegate func
    let myfunPtr = Marshal.GetFunctionPointerForDelegate(myfun)

    [<Test>]
    let ``[Fragment] execution working``() =
        let manager = MemoryManager.createExecutable()

        let maxArgs = 6
        let prolog = manager |> CodeFragment.prolog maxArgs 
        let epilog = manager |> CodeFragment.epilog maxArgs 



        let frag = 

            manager |> CodeFragment.ofCalls [myfunPtr, [||]]
                //myfunPtr, [|1 :> obj|]
                //myfunPtr, [|4 :> obj; 3 :> obj; 2 :> obj; 1 :> obj; 0 :> obj|]
                //]

        frag.Memory.UInt8Array |> Array.map (sprintf "0x%x") |> String.concat ", " |> printfn "{ %s }"

            
        prolog.WriteNextPointer frag.Offset |> ignore
        frag.WriteNextPointer epilog.Offset |> ignore

        //prolog.WriteNextPointer epilog.Offset |> ignore

        frag.ReadNextPointer() |> should equal epilog.Offset
        prolog.ReadNextPointer() |> should equal frag.Offset

        //Console.WriteLine("Code:")
        //let instructions = frag.Calls
        //frag.Memory.UInt8Array |> Array.iter (printfn "%x")
        //for (ptr, args) in instructions do
        //    Console.WriteLine("  {0}({1})", sprintf "%A" ptr, sprintf "%A" args)

        let run1 = UnmanagedFunctions.wrap myfunPtr
        run1(1)


        let run = CodeFragment.wrap prolog
        run()



        myfun.Invoke(1)

        //calls |> Seq.toList 
        //      |> should equal [
        //        [|1 :> obj; 2 :> obj; 3 :> obj; 4 :> obj; 5 :> obj|]
        //        [|4 :> obj; 3 :> obj; 2 :> obj; 1 :> obj; 0 :> obj|]
        //      ]

        ()

    let run() = ``[Fragment] execution working``()