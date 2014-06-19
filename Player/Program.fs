open System
open System.Drawing
open System.Windows.Forms

open LazyList
open Shapes
open Cartoon

open Drawer

let animateWith positions c = Movie(eval positions, c)

let scene1 = shapes { yield! [Point.Origin, Rectangle(10, 10, Color.Blue)
                              {x = 8; y = 60; z=0}, Ellipse(16, 8, Color.Red)
                              {x = 8; y = 60; z=0}, Ellipse(8, 16, Color.Red)] }

let clip1 = clip { yield {x=100; y=100; z=0}, scene1
                   yield {x=125; y=100; z=0}, scene1
                   yield {x=150; y=100; z=0}, scene1
                   yield {x=175; y=100; z=0}, scene1 }

let movie1 = clip1 |> animateWith (lazylist { for i in 1..10 do
                                              yield {x=100; y=i; z=0} })

let rec repeat s = seq { yield! s 
                         yield! repeat s }

let movie2 =
    clip1
    |> animateWith
        (lazylist {
                 yield! lazylist { for i in 1..25 do yield {x=0; y=i; z=0} }
                 yield! lazylist { for i in 1..25 do yield {x=i; y=25; z=0} }
                 yield! lazylist { for i in 1..25 do yield {x=25; y=25-i; z=0} }
                 yield! lazylist { for i in 1..25 do yield {x=25-i; y=0; z=0} }
             }
         |> LazyList.repeat) 

let clip3 = clip { yield! clip1
                   yield! movie2 }

let movie3 =
    clip3
    |> animateWith
        (lazylist {
            for i in 1..100 do
            yield {x=0; y=i; z=0}
         })

let cartoon = movie3

[<EntryPoint>]
[<STAThread>]
let main argv = 
    let w = new Form()
    w.Text <- "Cartoon test"
    w.Width <- 640 + w.Width - w.ClientSize.Width
    w.Height <- 480 + w.Height - w.ClientSize.Height

    let canvas = new PictureBox()
    canvas.Top <- 0
    canvas.Left <- 0
    canvas.Size <- w.ClientSize

    w.Controls.Add(canvas)

    let graphics = canvas.CreateGraphics()

    let c = ref(Some(cartoon))

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
