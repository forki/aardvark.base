#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Testing


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Aardvark.sln"]

Target "Tests" (fun () ->
    NUnit3 (fun p -> { p with OutputDir = "tests.out" })  [ @"bin/Release/Aardvark.Base.Incremental.Tests.exe"; @"bin/Release/Aardvark.Base.Runtime.Tests.dll" ]
)

"Compile" ==> "Tests"

#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()
