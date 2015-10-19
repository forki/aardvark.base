namespace Aardvark.Base.FSharp.Tests

open System
open Aardvark.Base
open FsUnit
open NUnit.Framework
open System.Collections.Generic

module CollectionTests =


    [<Test>]
    let ``collection memory overhead``() =

        let creators =
            [
                "FSharpList<int>", fun n -> [1..n] :> obj
                "FSharpSet<int>", fun n -> Set.ofList [1..n] :> obj
                "FSharpMap<int, int>", fun n -> Map.ofList (List.init n (fun i -> i,i)) :> obj
                "List<int>", fun n -> System.Collections.Generic.List [1..n] :> obj
                "HashSet<int>", fun n -> HashSet [1..n] :> obj
                "DictSet<int>", fun n -> DictSet [1..n] :> obj
                "Dictionary<int, int>", fun n -> Dictionary.ofList (List.init n (fun i -> i,i)) :> obj
                "Dict<int, int>", fun n -> Dict.ofList (List.init n (fun i -> i,i)) :> obj
            ]

        let b = System.Text.StringBuilder()

        let header = "size;" + (creators |> List.map fst |> String.concat ";")
        b.AppendLine(header) |> ignore

        let printf fmt = Printf.kprintf(fun str -> Console.Write("{0}", str)) fmt
        let printfn fmt = Printf.kprintf(fun str -> Console.WriteLine("{0}", str)) fmt

        let cnt = 1000
        for s in 1..100..10000 do
            printfn "size: %d" s
            let mutable results = []
            for (n, c) in creators do
                let arr = Array.zeroCreate cnt

                let before = GC.GetTotalMemory(true)
                for i in 0..cnt-1 do
                    arr.[i] <- c s

                let after = GC.GetTotalMemory(true)

                let avg = float (after - before) / float cnt
                printfn "  %s: %.2fb" n avg


                results <- results @ [string avg]

                for i in 0..cnt-1 do
                    arr.[i] <- null

            let line = ((string s) :: results) |> String.concat ";"
            b.AppendLine(line) |> ignore
            

        System.IO.File.WriteAllText(@"C:\Users\schorsch\Desktop\test.csv", b.ToString())
    