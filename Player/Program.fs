open System
open System.Drawing
open System.Windows.Forms

open Drawer

let cartoon = FCartoon.SampleClips.test4

[<EntryPoint>]
[<STAThread>]
let main argv = 
    let c = ref(Some(cartoon))

    let w = new Form()
    w.Text <- "Cartoon test"
    w.Width <- 640 + w.Width - w.ClientSize.Width
    w.Height <- 480 + w.Height - w.ClientSize.Height

    let canvas = new PictureBox()
    canvas.Top <- 0
    canvas.Left <- 0
    canvas.Size <- w.ClientSize

    let restartButton = new Button()
    restartButton.Top <- 0
    restartButton.Left <- 0
    restartButton.Text <- "Restart"

    w.Controls.Add(restartButton)
    w.Controls.Add(canvas)

    let onRestartClick (o:Object) (e:EventArgs) =
        c := Some(cartoon)

    restartButton.Click.AddHandler(new EventHandler(onRestartClick))

    let graphics = canvas.CreateGraphics()

    let updatePicture (o:Object) (e:EventArgs) =
        match !c with
        | Some(c') -> play graphics c'
                      c := c'.GetNext()
        | _ -> ()

    let timer = new Timer()
    timer.Interval <- 42
    timer.Tick.AddHandler(new EventHandler(updatePicture))
    timer.Enabled <- true

    Application.Run(w)

    0 // return an integer exit code
