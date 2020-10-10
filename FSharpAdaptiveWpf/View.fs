namespace AdaptiveWpf


open System 
open System.Windows 
open System.IO
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Input
open System.Windows.Controls 
open FSharp.Data.Adaptive

open System.Reflection

module ViewHelpers =
  let inline watch (v: 'a aval option) fn =
    v |> Option.map (fun v -> v.AddWeakCallback fn)

  let watchList (v: 'a alist option) add set remove =
    v
    |> Option.map (fun v -> v.AddWeakCallback(fun list delta ->
      for (idx, op) in delta.ToSeq() do
        let i = list.IndexOf idx
        match op with
        | ElementOperation.Remove -> remove i
        | ElementOperation.Set r when i >= 0 -> set i r
        | ElementOperation.Set r -> add r
    ))

  let inline viewElementsDispose (viewElements: ViewElement alist option) =
    viewElements
    |> Option.map (
        fun viewElements ->
          { new IDisposable with
              member x.Dispose() = 
                
                ()
          })

  let inline commandCallback fn =
    let event = Event<EventHandler,EventArgs>()
    { new ICommand with
        [<CLIEvent>]
        member this.CanExecuteChanged: IEvent<EventHandler,EventArgs> = event.Publish
        member x.CanExecute _ = true
        member x.Execute _ = fn ()
    }
    
open ViewHelpers
type View private () =

  /// Describes a Button in the view
  static member inline Button
    (
      ?text: string aval,
      ?command: (unit -> unit) aval,
      ?background: Brush aval,
      ?borderColor: Brush aval,
      ?borderThickness: Thickness aval,
      ?flowDirection: FlowDirection aval,
      ?height: float aval,
      ?horizontalAlignment: HorizontalAlignment aval,
      ?isEnabled: bool aval,
      ?margin: Thickness aval,
      ?minimumHeight: float aval,
      ?minimumWidth: float aval,
      ?opacity: float aval,
      ?padding: Thickness aval,
      ?verticalAlignment: VerticalAlignment aval,
      ?width: float aval
    ) = 
      let v = new Button()
      let disposables =
        seq {
          watch text (fun x -> v.Content <- x)
          watch command (fun x -> v.Command <- commandCallback x)
          watch background (fun x -> v.Background <- x)
          watch borderColor (fun x -> v.BorderBrush <- x)
          watch borderThickness (fun x -> v.BorderThickness <- x)
          watch flowDirection (fun x -> v.FlowDirection <- x)
          watch height (fun x -> v.Height <- x)
          watch horizontalAlignment (fun x -> v.HorizontalAlignment <- x)
          watch isEnabled (fun x -> v.IsEnabled <- x)
          watch margin (fun x -> v.Margin <- x)
          watch minimumHeight (fun x -> v.MinHeight <- x)
          watch minimumWidth (fun x -> v.MinWidth <- x)
          watch opacity (fun x -> v.Opacity <- x)
          watch padding (fun x -> v.Padding <- x)
          watch verticalAlignment (fun x -> v.VerticalAlignment <- x)
          watch width (fun x -> v.Width <- x)
        }|> Seq.choose id |> Array.ofSeq
             
      new ViewElement(v :> UIElement, disposables)

  static member inline Grid
    (
      ?children: ViewElement alist,
      ?background: Brush aval,
      ?coldefs: ColumnDefinition alist,
      ?flowDirection: FlowDirection aval,
      ?height: float aval,
      ?horizontalAlignment: HorizontalAlignment aval,
      ?margin: Thickness aval,
      ?minimumHeight: float aval,
      ?minimumWidth: float aval,
      ?opacity: float aval,
      ?rowdefs: RowDefinition alist,
      ?verticalAlignment: VerticalAlignment aval,
      ?width: float aval
    ) = 
      let v = new Grid()
      let disposables =
        seq {
          watch background (fun x -> v.Background <- x)

          watchList coldefs 
            (fun x -> v.ColumnDefinitions.Add(x))
            (fun idx x -> v.ColumnDefinitions.RemoveAt(idx); v.ColumnDefinitions.Insert(idx, x))
            (fun idx -> v.ColumnDefinitions.RemoveAt(idx))

          watch flowDirection (fun x -> v.FlowDirection <- x)
          watch height (fun x -> v.Height <- x)
          watch horizontalAlignment  (fun x -> v.HorizontalAlignment <- x)
          watch margin (fun x -> v.Margin <- x)
          watch minimumHeight (fun x -> v.MinHeight <- x)
          watch minimumWidth (fun x -> v.MinWidth <- x)
          watch opacity (fun x -> v.Opacity <- x)

          watchList rowdefs 
            (fun x -> v.RowDefinitions.Add(x))
            (fun idx x -> v.RowDefinitions.RemoveAt(idx); v.RowDefinitions.Insert(idx, x))
            (fun idx -> v.RowDefinitions.RemoveAt(idx))

          watch verticalAlignment (fun x -> v.VerticalAlignment <- x)
          watch width (fun x -> v.Width <- x)

          watchList children
            (fun x -> v.Children.Add(x.UIElement) |> ignore)
            (fun idx x -> v.Children.RemoveAt(idx); v.Children.Insert(idx, x.UIElement))
            (fun idx -> v.Children.RemoveAt(idx))
          viewElementsDispose children


        } |> Seq.choose id |> Array.ofSeq
      
      new ViewElement(v :> UIElement, disposables)