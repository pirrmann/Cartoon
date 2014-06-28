module SampleClips

open FCartoon
open FCartoon.LazyList
open FCartoon.Builders

let scene1 = shapes { yield! [RefSpace.Origin, ClosedShape(Rectangle(Vector(10.0, 10.0)), Fill Brush.Blue)
                              RefSpace.At(8.0, 60.0), ClosedShape(Ellipse(Vector(16.0, 8.0)), Fill Brush.Red)
                              RefSpace.At(8.0, 60.0), ClosedShape(Ellipse(Vector(8.0, 16.0)), Fill Brush.Red)] }

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
         |> FCartoon.LazyList.repeat) 

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
        RefSpace.Origin, (ClosedShape(Rectangle(Vector(200.0, 100.0)), Contour Pen.Red))
        RefSpace.At(100.0, 0.0), (ClosedShape(Rectangle(Vector(200.0, 100.0)), Contour Pen.Blue))
    ]

let testRotate =
    Frame(RefSpace.Origin, rects)
    |> transformWith
        (lazylist {
            for i in 0..90 do
            yield { RefSpace.Origin with transform = Transforms.rotate(float i * System.Math.PI / 180.0) }
         })

let head =
    clips {
        yield RefSpace.Origin,
              shapes {
                yield RefSpace.Origin, ClosedShape(Ellipse(Vector(300.0, 300.0)), Contour Pen.Red)
                yield RefSpace.At(-50.0, -230.0), ClosedShape(Rectangle(Vector(100.0, 100.0)), Fill Brush.Black)
                yield RefSpace.At(0.0, -130.0), ClosedShape(Ellipse(Vector(150.0, 50.0)), Fill Brush.Black)
                yield RefSpace.At(50.0, -50.0), ClosedShape(Ellipse(Vector(40.0, 40.0)), Fill Brush.Blue)
                yield RefSpace.At(-50.0, -50.0), ClosedShape(Ellipse(Vector(80.0, 80.0)), Fill Brush.Blue)
                yield RefSpace.At(10.0, -20.0), Path(Bezier(Vector(0.0, 40.0), Vector(-30.0, 30.0), Vector(-15.0, 15.0)), { Pen.Red with Thickness = 5.0 }) }
        yield! Clip(
                    RefSpace.At(0.0, 50.0),
                    lazylist { for i in 10 .. 40 do
                               yield Frame(RefSpace.At(-50.0, 0.0), [RefSpace.Origin, Path((Bezier(Vector(100.0, 0.0), Vector(float i, float i), Vector(float (-i), float i)), { Pen.Green with Thickness = 5.0 }))]) } |> eval |> holdOnLast)
    }

let test4 =
    Clips(RefSpace.At(0.0, 0.0), [head])
    |> transformWith
        (lazylist {
            for i in 1..96 do
            yield { RefSpace.Origin with transform = Transforms.rotate(sin (float i * System.Math.PI / 12.0) / 3.0) }
         })
    |> transformWith
        (lazylist {
            for i in 1..96 do
            let scaleRatio = min 1.2 (float i / 48.0)
            let y = if i > 48 then float (i - 48) else 0.0
            yield { RefSpace.Origin with transform = Transforms.scale(scaleRatio) * Transforms.translate(0.0, y) }
         })

open Dsl

let scene2 = shapes {
    yield rectangle (100.0, 100.0) |> withContour Pen.Blue |> at origin
    yield ellipse (100.0, 100.0) |> withContourAndFill ({Pen.Black with Thickness = 5.0}, Brush.Red) |> at (-50.0, -50.0)
    yield line (0.0, 0.0) (100.0, 100.0) |> withPen Pen.Green
    yield bezier (0.0, 0.0) (100.0, 100.0) (0.0, 50.0) (0.0, -50.0) |> withPen { Pen.Red with Thickness = 3.0 }
    }

let test5 = Frame(RefSpace.Origin, scene2)