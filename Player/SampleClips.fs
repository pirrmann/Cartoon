module SampleClips

open LazyList
open Shapes
open Cartoon

let scene1 = shapes { yield! [RefSpace.Origin, RectangleFill(Vector(10.0, 10.0), Brush.Blue)
                              RefSpace.At(8.0, 60.0), EllipseFill(Vector(16.0, 8.0), Brush.Red)
                              RefSpace.At(8.0, 60.0), EllipseFill(Vector(8.0, 16.0), Brush.Red)] }

let clip1 = clips { yield RefSpace.At(100.0, 100.0), scene1
                    yield RefSpace.At(125.0, 100.0), scene1
                    yield RefSpace.At(150.0, 100.0), scene1
                    yield RefSpace.At(175.0, 100.0), scene1 }

let movie1 = clip1 |> transformWith (lazylist { for i in 1..10 do
                                                yield RefSpace.At(100.0, float i) })

let rec repeat s = seq { yield! s 
                         yield! repeat s }

let movie2 =
    clip1
    |> transformWith
        (lazylist {
                 yield! lazylist { for i in 1..25 do yield RefSpace.At(0.0, float i) }
                 yield! lazylist { for i in 1..25 do yield RefSpace.At(float i, 25.0) }
                 yield! lazylist { for i in 1..25 do yield RefSpace.At(25.0, 25.0 - float i) }
                 yield! lazylist { for i in 1..25 do yield RefSpace.At(25.0 - float i, 0.0) }
             }
         |> LazyList.repeat) 

let clip3 = clips { yield! clip1
                    yield! movie2 }

let movie3 =
    clip3
    |> transformWith
        (lazylist {
            for i in 1..100 do
            yield RefSpace.At(0.0, float i)
         })

let rects =
    [
        RefSpace.Origin, Rectangle(Vector(200.0, 100.0), Pen.Red)
        RefSpace.At(100.0, 0.0), Rectangle(Vector(200.0, 100.0), Pen.Blue)
    ]

let testRotate =
    Frame(RefSpace.Origin, rects)
    |> transformWith
        (lazylist {
            for i in 0..90 do
            yield { RefSpace.Origin with transform = rotate(float i * System.Math.PI / 180.0) }
         })

let head =
    clips {
        yield RefSpace.Origin,
              shapes {
                yield RefSpace.Origin, Ellipse(Vector(300.0, 300.0), Pen.Red)
                yield RefSpace.At(-50.0, -220.0), RectangleFill(Vector(100.0, 100.0), Brush.Black)
                yield RefSpace.At(50.0, -50.0), EllipseFill(Vector(20.0, 20.0), Brush.Blue)
                yield RefSpace.At(-50.0, -50.0), EllipseFill(Vector(20.0, 20.0), Brush.Blue) }
        yield! Clip(
                    RefSpace.At(0.0, 50.0),
                    lazylist { for i in 10 .. 40 do
                               yield Frame(RefSpace.At(-50.0, 0.0), [RefSpace.Origin, Bezier(Vector(100.0, 0.0), Vector(float i, float i), Vector(float (-i), float i), { Pen.Green with Thickness = 5 })]) } |> eval |> holdOnLast)
    }

let test4 =
    Clips(RefSpace.At(0.0, 0.0), [head])
    |> transformWith
        (lazylist {
            for i in 1..96 do
            yield { RefSpace.Origin with transform = rotate(sin (float i * System.Math.PI / 12.0) / 3.0) }
         })
    |> transformWith
        (lazylist {
            for i in 1..96 do
            let scaleRatio = min 1.2 (float i / 48.0)
            let y = if i > 48 then float (i - 48) else 0.0
            yield { RefSpace.Origin with transform = scale(scaleRatio) * translate(0.0, y) }
         })
