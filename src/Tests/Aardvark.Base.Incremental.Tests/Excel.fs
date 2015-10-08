namespace Aardvark.Base

open Microsoft.Office.Interop.Excel
open Aardvark.Base
open Aardvark.Base.Incremental
open NUnit.Framework
open FsUnit
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Reflection




module Excel =
    

    let fieldNames<'a> =
        let t = typeof<'a>
        if FSharpType.IsRecord t then
            t |> FSharpType.GetRecordFields |> Seq.map (fun fi -> fi.Name) |> Seq.toList
        else
            ["value"]

    let toArray<'a> =
        let t = typeof<'a>
        if FSharpType.IsRecord t then
            let fields = t |> FSharpType.GetRecordFields
            fun (value : 'a) ->
                fields |> Array.map (fun p -> p.GetValue(value))
        else
            fun value -> [|value :> obj|]
        

    let start (f : unit -> unit) =
        Task.Factory.StartNew(f, TaskCreationOptions.LongRunning) |> ignore

    let rec colName (i : int) =
        if i < 26 then 'A' + char i |> string
        else 
            let head = colName (i / 26 - 1)
            let tail = colName (i % 26)
            head + tail

    let rowName (i : int) =
        i + 1 |> string

    let cellName (row : int) (col : int) =
        colName col + rowName row

    let visualize (data : amap<int, 'a>) =
        
        let app = ApplicationClass()
        app.Visible <- true

        let book = app.Workbooks.Add()
        let sheet = book.ActiveSheet |> unbox<Worksheet>
        
        // write the header
        sheet.Cells.[1,1] <- "N"
        let header = fieldNames<'a>
        for i in 0..header.Length-1 do
            sheet.Cells.[1, 2 + i] <- header.[i]
            (unbox<Range> sheet.Columns.[2 + i]).AutoFit() |> ignore

        let r = data.ASet.GetReader()

        let arr = toArray<'a>
        let freeRow = ref 2
        start (fun () ->
            while true do
                
                let delta = r.GetDelta()

                if not <| List.isEmpty delta then
                    for d in delta do
                        match d with
                            | Add (n,value) ->
                                let values = arr value
                                let r = !freeRow

                                sheet.Cells.[r, 1] <- n
                                for i in 0..values.Length-1 do
                                    sheet.Cells.[r, 2 + i] <- values.[i]


                                freeRow := !freeRow + 1
                            | Rem _ -> ()
                Thread.Sleep 50
        )

        

        let charts = sheet.ChartObjects() |> unbox<ChartObjects>
        let chart = sheet.Shapes.AddChart().Chart //charts.Add(300.0, 50.0, 400.0, 300.0).Chart

        chart.ChartWizard(
            Gallery = XlChartType.xlXYScatterLinesNoMarkers
        )
        let missing = null
        chart.ChartWizard(
            Source = null,
            Gallery = XlChartType.xlXYScatterSmoothNoMarkers, 
            CategoryTitle = "N"
        )

        let series = chart.SeriesCollection() |> unbox<SeriesCollection>
        let nRange = sheet.Range(cellName 1 0, cellName 10000 0)
        for i in 0..header.Length-1 do    
            let range = sheet.Range(cellName 1 i, cellName 10000 i)
            let series = series.Add(range)
            series.XValues <- nRange
            series.MarkerStyle <- XlMarkerStyle.xlMarkerStyleNone
            series.Name <- header.[i]


//        chart.ChartWizard(
//            Title = "asdasd",
//            Source = sheet.Range("A2", "C100"),
//            Gallery = XlChartType.xlXYScatterLinesNoMarkers,
//            //PlotBy = XlRowCol.xlColumns,
//            SeriesLabels = sheet.Range("A1", "C1")
//        )
//        
//
//        chart.ChartStyle <- 2

        let tcs = TaskCompletionSource<unit>()
        let handler = WorkbookEvents_BeforeCloseEventHandler(fun (r : byref<bool>) -> tcs.SetResult(()))
        book.add_BeforeClose handler
        tcs.Task


    type Result = { submitTime : float; pullTime : float }

    [<Test>]
    let ``[Excel] simple visualization``() =
        let data = 
            CMap.ofList [
                1, { submitTime = 1.0; pullTime = 2.0 }
                2, { submitTime = 2.0; pullTime = 3.0 }
            ]

        start (fun () ->

            for i in 3..20 do
                transact (fun () ->
                    data |> CMap.add i { submitTime = float i; pullTime = float i }
                )
                Thread.Sleep(500)
        
        )

        let t = visualize data

        t.Result

        ()
