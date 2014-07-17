namespace FCartoon

[<ReflectedDefinition>]
module SampleClips =

    open LazyList
    open Builders

    let scene1 = shapes { yield! [RefSpace.Origin, ClosedShape(Rectangle(Vector(10.0, 10.0)), Fill Brushes.Blue)
                                  RefSpace.At(8.0, 60.0), ClosedShape(Ellipse(Vector(16.0, 8.0)), Fill Brushes.Red)
                                  RefSpace.At(8.0, 60.0), ClosedShape(Ellipse(Vector(8.0, 16.0)), Fill Brushes.Red)] }

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
            RefSpace.Origin, (ClosedShape(Rectangle(Vector(200.0, 100.0)), Contour Pens.Red))
            RefSpace.At(100.0, 0.0), (ClosedShape(Rectangle(Vector(200.0, 100.0)), Contour Pens.Blue))
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
                    yield RefSpace.Origin, ClosedShape(Ellipse(Vector(300.0, 300.0)), Contour Pens.Red)
                    yield RefSpace.At(-50.0, -230.0), ClosedShape(Rectangle(Vector(100.0, 100.0)), Fill Brushes.Black)
                    yield RefSpace.At(0.0, -130.0), ClosedShape(Ellipse(Vector(150.0, 50.0)), Fill Brushes.Black)
                    yield RefSpace.At(50.0, -50.0), ClosedShape(Ellipse(Vector(40.0, 40.0)), Fill Brushes.Blue)
                    yield RefSpace.At(-50.0, -50.0), ClosedShape(Ellipse(Vector(80.0, 80.0)), Fill Brushes.Blue)
                    yield RefSpace.At(10.0, -20.0), Path(Bezier(Vector(0.0, 40.0), Vector(-30.0, 30.0), Vector(-15.0, 15.0)), { Pens.Red with Thickness = 5.0 }) }
            yield! Clip(
                        RefSpace.At(0.0, 50.0),
                        lazylist { for i in 10 .. 40 do
                                   yield Frame(RefSpace.At(-50.0, 0.0), [RefSpace.Origin, Path((Bezier(Vector(100.0, 0.0), Vector(float i, float i), Vector(float (-i), float i)), { Pens.Green with Thickness = 5.0 }))]) } |> eval |> holdOnLast)
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
        yield rectangle (100.0, 100.0) |> at origin |> withContour Pens.Blue
        yield ellipse (100.0, 100.0) |> at (-50.0, -50.0) |> withContourAndFill ({Pens.Black with Thickness = 5.0}, Brushes.Red)
        yield line (0.0, 0.0) (100.0, 100.0) |> withPen Pens.Green
        yield bezier (0.0, 0.0) (100.0, 100.0) (0.0, 50.0) (0.0, -50.0) |> withPen { Pens.Red with Thickness = 3.0 }
        }

    let test5 = scene2 |> at origin |> Frame

    let hearts = shapes {
        yield [ bezierTo (0.0, -100.0) (-100.0, -100.0) (-10.0, -75.0)
                bezierTo (0.0, 100.0) (10.0, -75.0) (100.0, -100.0) ] |> toPath |> at origin |> withPen Pens.Red 
        yield [ bezierTo (10.0, -100.0) (-100.0, -100.0) (-10.0, -75.0)
                bezierTo (10.0, 100.0) (10.0, -75.0) (100.0, -100.0)
                bezierTo (-20.0, 0.0) (-10.0, 10.0) (10.0, 10.0) ] |> toClosedPath |> at (-200.0, 0.0) |> withContourAndFill ({Pens.Black with Thickness = 3.0}, Brushes.Pink)
    }

    let gruyere = shapes {
        let hole1 = rectangle (40.0, 40.0) |> at (10.0, 10.0)
        yield
            rectangle (100.0, 100.0)
            |> withHole hole1
            |> withHole (rectangle (20.0, 20.0) |> at (68.0, 50.0) |> rotatedBy (Pi / 4.0))
            |> withHole (ellipse (20.0, 20.0) |> at (25.0, 75.0))
            |> at origin
            |> withContourAndFill (Pens.Blue, Brushes.BlueViolet)
        yield
            hole1
            |> withFill {Brushes.Solid with Color = { Colors.Yellow with Alpha = 0.2 }}
    }

    let movingHole =
        lazylist {
            for i in 1..96 do
            yield
                shapes {
                    let hole = (ellipse (90.0 * (1.0 + sin(float i * Pi / 48.0)), 90.0 * (1.0 + sin(float i * Pi / 48.0 + Pi))) |> at (100.0, 100.0)) 
                    yield
                        rectangle (200.0, 200.0)
                        |> withHole hole
                        |> at origin
                        |> withContourAndFill (Pens.Black, Brushes.DarkGray)
                    yield
                        hole
                        |> withContourAndFill (Pens.Black, { Brushes.Aqua with Color = { Colors.Aqua with Alpha = 0.2 } })
                } |> at (-100.0, -100.0) |> Frame
        } |> LazyList.repeat |> eval |> at origin |> withZ 3.0 |> Clip
        |> transformWith
            (lazylist {
                for i in 1..36 do
                    yield RefSpace.At(20.0 * cos(float i * Pi / 18.0), 20.0 * sin(float i * Pi / 18.0))
             } |> LazyList.repeat)
        |> transformWith
            (lazylist {
                for i in 1..100 do
                    yield RefSpace.At(0.0, float i)
                for i in 1..100 do
                    yield RefSpace.At(float i, 100.0)
                for i in 1..100 do
                    yield RefSpace.At(100.0, 100.0 - float i)
                for i in 1..100 do
                    yield RefSpace.At(100.0 - float i, 0.0)
             } |> LazyList.repeat)

    let test6 =
        Clips(
            RefSpace.Origin,
            [
                hearts |> at origin |> withZ 1.0 |> Frame
                gruyere |> at origin |> withZ 2.0 |> Frame |> transformWith
                    (lazylist {
                        for i in 1..48 do
                        yield { RefSpace.Origin with transform = Transforms.rotate(Pi / 4.0 + sin (float i * Pi / 12.0)) * Transforms.translate(-190.0, -120.0) }
                     } |> LazyList.repeat)
                movingHole
            ])
    
