namespace FCartoon

[<ReflectedDefinition>]
module SampleClips =

    open LazyList
    open Builders

    let scene1 = shapes { yield RefSpace.Origin, ClosedShape(Rectangle(Vector(10.0, 10.0)), Fill Brushes.Blue)
                          yield RefSpace.At(8.0, 60.0), ClosedShape(Ellipse(Vector(16.0, 8.0)), Fill Brushes.Red)
                          yield RefSpace.At(8.0, 60.0), ClosedShape(Ellipse(Vector(8.0, 16.0)), Fill Brushes.Red) }

    let clip1 = clips { yield RefSpace.At(100.0, 100.0), scene1
                        yield RefSpace.At(125.0, 100.0), scene1
                        yield RefSpace.At(150.0, 100.0), scene1
                        yield RefSpace.At(175.0, 100.0), scene1 }

    let movie1 = clip1 |> transformWith (lazylist { for i in 1..10 do
                                                    yield RefSpace.At(100.0, float i) })

    let movie2 =
        clip1
        |> transformWith
            (lazylist {
                     yield! lazylist { for i in 1..25 do yield RefSpace.At(0.0, float i) }
                     yield! lazylist { for i in 1..25 do yield RefSpace.At(float i, 25.0) }
                     yield! lazylist { for i in 1..25 do yield RefSpace.At(25.0, 25.0 - float i) }
                     yield! lazylist { for i in 1..25 do yield RefSpace.At(25.0 - float i, 0.0) }
                 }
             |> repeat) 

    let clip3 = clips { yield! RefSpace.Origin, clip1
                        yield! RefSpace.Origin, movie2 }

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
            yield! RefSpace.Origin, Clip(
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
    
    let movingBezier =
        let p1 = ref (Vector(0.0, 0.0))
        let p2 = ref (Vector(100.0, 50.0))
        let p3 = ref (Vector(0.0, 100.0))
        let d1 = ref (Vector(3.0, 2.0))
        let d2 = ref (Vector(-2.0, 5.0))
        let d3 = ref (Vector(5.0, -3.0))
        let t1 = ref (Vector(20.0, -20.0))
        let t2 = ref (Vector(0.0, 30.0))
        let t3 = ref (Vector(-20.0, -20.0))
        let step (d:Vector) (v:Vector) =
            let x, dx = if v.X + d.X > 640.0 || v.X + d.X < 0.0
                            then v.X - d.X, -d.X
                            else v.X + d.X, d.X
            let y, dy = if v.Y + d.Y > 480.0 || v.Y + d.Y < 0.0
                            then v.Y - d.Y, -d.Y
                            else v.Y + d.Y, d.Y
            Vector(x, y), Vector(dx, dy)

        let rec next () =
            lazylist {
                yield
                    shapes {
                        yield [ bezierTo (p2.Value.X - p1.Value.X, p2.Value.Y - p1.Value.Y) (t1.Value.X, t1.Value.Y) (-t2.Value.X, -t2.Value.Y)
                                bezierTo (p3.Value.X - p2.Value.X, p3.Value.Y - p2.Value.Y) (t2.Value.X, t2.Value.Y) (-t3.Value.X, -t3.Value.Y)
                                bezierTo (p1.Value.X - p3.Value.X, p1.Value.Y - p3.Value.Y) (t3.Value.X, t3.Value.Y) (-t1.Value.X, -t1.Value.Y) ] 
                              |> toClosedPath
                              |> at (p1.Value.X, p1.Value.Y)
                              |> withContourAndFill ({Pens.DarkGreen with Thickness = 3.0}, Brushes.LimeGreen)
                    } |> at (-320.0, -240.0) |> Frame
                let p1', d1' = !p1 |> step !d1
                p1 := p1'
                d1 := d1'
                let p2', d2' = !p2 |> step !d2
                p2 := p2'
                d2 := d2'
                let p3', d3' = !p3 |> step !d3
                p3 := p3'
                d3 := d3'
                t1 := Vector(50.0, 50.0)
                t2 := Vector(-50.0, -50.0)
                t3 := Vector(50.0, -50.0)
                yield! next ()
            }

        next () |> eval |> at origin |> Clip

    let test7 =
        shapes {
            yield rectangle (100.0, 100.0) |> at (-50.0, -50.0) |> withContour Pens.Blue
        } |> at (-100.0, -100.0) |> Frame
        |> transformWith (
            (
             (25 |> framesOf (slide (50.0, 50.0)))
             |> followedBy (50 |> framesOf (slide (-50.0, -50.0)))
             |> followedBy (25 |> framesOf (slide (100.0, -50.0)))
             |> followedBy (50 |> framesOf (slide (-100.0, 50.0)))
            ) |> repeat
           )

    let test8 =
        let tree =
            shapes {
                yield rectangle (10.0, 10.0) |> at (-5.0, -10.0) |> withFill Brushes.Maroon
                yield [ lineTo (-20.0, 0.0)
                        lineTo (20.0, -100.0)
                        lineTo (20.0, 100.0)
                        lineTo (-20.0, 0.0)
                      ]
                        |> toClosedPath
                        |> at (0.0, -10.0)
                        |> withContourAndFill ({Pens.DarkOliveGreen with Thickness = 2.0}, Brushes.DarkGreen)
            }

        let hero =
            let arm =
                shapes {
                    yield rectangle (1.5, 4.0) |> at (-0.75, 0.0) |> withFill Brushes.Red
                    yield ellipse (1.5, 1.5) |> at (0.0, 4.5) |> withZ (-0.1) |> withFill Brushes.LightPink
                }
            shapes {
                yield rectangle (10.0, 10.0) |> at (-5.0, -15.0) |> withFill Brushes.Red
                yield rectangle (2.0, 5.0) |> at (-5.0, -5.0) |> withFill Brushes.Red
                yield rectangle (2.0, 5.0) |> at (3.0, -5.0) |> withFill Brushes.Red
                yield! arm |> at (4.5, -14.0) |> rotatedBy (-Pi / 6.0)
                yield! arm |> at (-4.5, -14.0) |> rotatedBy (Pi / 6.0)
                yield resource "guybrush.png" |> image (52.0, 47.0) |> at (-4.0, -23.0) |> scaledBy 0.2
            }

        let forest =
            clips {
                yield tree |> at (-50.0, 4.0) |> scaledBy 0.98 |> withZ 1.00
                yield tree |> at (-30.0, 13.0) |> scaledBy 1.1 |> withZ 2.00
                yield tree |> at (-20.0, 4.0) |> scaledBy 0.98 |> withZ 1.01
                yield tree |> at (0.0, 0.0) |> scaledBy 1.0 |> withZ 1.02
                yield tree |> at (20.0, 8.0) |> scaledBy 1.05 |> withZ 2.01
                yield tree |> at (30.0, 13.0) |> scaledBy 1.1 |> withZ 2.02
                yield tree |> at (40.0, -4.0) |> scaledBy 0.95 |> withZ 1.03
                yield tree |> at (60.0, 13.0) |> scaledBy 1.1 |> withZ 2.03
                yield tree |> at (100.0, 13.0) |> scaledBy 1.1 |> withZ 2.04
                yield tree |> at (150.0, 13.0) |> scaledBy 1.1 |> withZ 2.05
            }

        let walkingHero =
            hero |> at (0.0, 0.0) |> Frame
            |> transformWith (
             (
              (30 |> framesOf (slideWithZ (250.0, 40.0, 3.0)))
              |> followedBy ((30 |> framesOf (slideWithZ (250.0, -20.0, -1.5))))
              |> followedBy ((12 |> framesOf (slideWithZ (10.0, -30.0, 0.0) >> whileApplying Transforms.flipX)))
              |> followedBy ((30 |> framesOf (slideWithZ (240.0, -20.0, -1.5))))
              |> followedBy ((20 |> framesOf (slideWithZ (150.0, -10.0, -1.0))))
              |> followedBy ((15 |> framesOf (slideWithZ (100.0, 10.0, 1.0))))
              |> followedBy ((12 |> framesOf (slideWithZ (10.0, 30.0, 0.0))))
             ) |> repeat
            )

        let background =
            shapes {
                yield [ lineTo (640.0, 0.0)
                        lineTo (0.0, 190.0)
                        bezierTo (-640.0, -50.0) (-100.0, 0.0) (100.0, 0.0)
                        lineTo (0.0, -140.0) ] |> toClosedPath |> at (-320.0, -240.0) |> withFill Brushes.SkyBlue
                yield [ bezierTo (640.0, 50.0) (100.0, 0.0) (-100.0, 0.0)
                        lineTo (0.0, 110.0)
                        bezierTo (-320.0, -40.0) (-80.0, 0.0) (100.0, 0.0)
                        bezierTo (-320.0, -30.0) (-80.0, 0.0) (100.0, 0.0)
                        lineTo (0.0, -90.0) ] |> toClosedPath |> at (-320.0, -100.0) |> withFill Brushes.LightGreen
                yield [ bezierTo (320.0, 30.0) (100.0, 0.0) (-80.0, 0.0)
                        bezierTo (320.0, 40.0) (100.0, 0.0) (-80.0, 0.0)
                        lineTo (0.0, 40.0)
                        bezierTo (-320.0, -40.0) (-80.0, 0.0) (100.0, 0.0)
                        bezierTo (-320.0, -30.0) (-80.0, 0.0) (100.0, 0.0)
                        lineTo (0.0, -40.0) ] |> toClosedPath |> at (-320.0, -10.0) |> withFill Brushes.SandyBrown
                yield [ bezierTo (320.0, 30.0) (100.0, 0.0) (-80.0, 0.0)
                        bezierTo (320.0, 40.0) (100.0, 0.0) (-80.0, 0.0)
                        lineTo (0.0, 150.0)
                        lineTo (-640.0, 0.0)
                        lineTo (0.0, -210.0) ] |> toClosedPath |> at (-320.0, 30.0) |> withFill Brushes.LightGreen
            }

        clips {
            yield background |> at origin |> withZ -512.0
            yield! forest |> at (-150.0, -60.0) |> withZ -10.0 |> scaledBy 0.9
            yield! forest |> at origin
            yield! forest |> at (150.0, -50.0) |> withZ -5.0 |> scaledBy 0.95
            yield! forest |> at (-220.0, 100.0) |> withZ 5.0 |> scaledBy 1.05
            yield! walkingHero |> at (-220.0, 5.0)
        }

