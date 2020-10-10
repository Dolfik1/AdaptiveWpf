module Program


open System 
open System.Windows 
open System.IO
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Controls 
open System.Reflection
open AdaptiveWpf
open FSharp.Data.Adaptive

type MainWindow() =
  inherit Window()

let window = MainWindow()


type Model = { Value: int cval }

type Message =
  | Increment
  | Decrement

let update msg (model: Model) =
  match msg with
  | Increment ->
    transact (fun _ -> model.Value.Value <- model.Value.Value + 1)

  | Decrement ->
    transact (fun _ -> model.Value.Value <- model.Value.Value - 1)

let model = { Value = cval 0 }

let view (model: Model) dispatch =
  View.Grid(
    background = (SolidColorBrush(Color.FromRgb(0uy, 0uy, 255uy)) :> Brush |> cval),

    coldefs = ([
      ColumnDefinition()
      ColumnDefinition()
    ] |> AList.ofList),
    
    
    rowdefs = ([
      RowDefinition()
      RowDefinition()
      RowDefinition()
    ] |> AList.ofList),

    children = ([



      View.Button(
        text = (model.Value |> AVal.map string)
      ).Row(1).Column(0).ColumnSpan(2)
      
      View.Button(
        text = (cval "Increment"),
        command = (cval (fun _ -> dispatch Increment))
      ).Row(2).Column(0)
      
      View.Button(
        text = (cval "Decrement"),
        command = (cval (fun _ -> dispatch Decrement))
      ).Row(2).Column(1)

    ] |> AList.ofList)
  )

do window.Content <- (view model (fun x -> update x model)).UIElement

[<STAThread>] 
[<EntryPoint>]
do (new Application()).Run(window) |> ignore