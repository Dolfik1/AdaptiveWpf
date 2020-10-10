namespace AdaptiveWpf

open System 
open System.Windows 
open System.IO
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Controls 
open FSharp.Data.Adaptive

type ViewElement(uiElement: UIElement, disposables: IDisposable[]) =
  
  member x.UIElement = uiElement

  member x.Row(i) =
    Grid.SetRow(uiElement, i)
    x

  member x.Column(i) =
    Grid.SetColumn(uiElement, i)
    x

  member x.ColumnSpan(i) =
    Grid.SetColumnSpan(uiElement, i)
    x
    
  member x.RowSpan(i) =
    Grid.SetRowSpan(uiElement, i)
    x

  interface IDisposable with
    member x.Dispose() =
      disposables |> Array.iter (fun x -> x.Dispose())