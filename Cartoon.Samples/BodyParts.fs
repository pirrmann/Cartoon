namespace FCartoon

[<ReflectedDefinition>]
module BodyParts =

    open LazyList
    open Builders
    open Dsl

    let hair = shapes {
        yield [ bezierTo (0.075, 0.075) (Kappa * 0.075, 0.0) (0.0, - Kappa * 0.075)
                bezierTo (-0.04, -0.03) (-0.01, 0.0) (0.0, 0.01)
                bezierTo (-0.11, 0.03) (-0.05, -0.02) (0.07, -0.02)
                bezierTo (0.075, -0.075) (0.0, - Kappa * 0.075) (- Kappa * 0.075, 0.0) ]
                |> toClosedPath |> at origin |> withFill Brushes.Maroon
    }

    let glasses = shapes {
        yield circle 0.02 |> at (-0.03, 0.0) |> withContour { Pens.Black with Thickness = 0.003 }
        yield circle 0.02 |> at (0.03, 0.0) |> withContour { Pens.Black with Thickness = 0.003 }
        yield [lineTo (0.02, 0.0)] |> toPath |> at (-0.01, 0.0) |> withPen { Pens.Black with Thickness = 0.003 }
        yield [lineTo (-0.02, -0.02)] |> toPath |> at (-0.05, 0.0) |> withZ (-0.01) |> withPen { Pens.Black with Thickness = 0.003 }
        yield [lineTo (0.02, -0.02)] |> toPath |> at (0.05, 0.0) |> withZ (-0.01) |> withPen { Pens.Black with Thickness = 0.003 }
    }

    let eyepatch = shapes {
        yield [ bezierTo (0.02, -0.01) (0.0, 0.0) (-0.01, 0.0)
                bezierTo (0.02, 0.01) (0.01, 0.0) (0.0, 0.0)
                bezierTo (-0.02, 0.025) (0.0, 0.01) (0.01, 0.0)
                bezierTo (-0.02, -0.025) (-0.01, 0.0) (0.0, 0.01) ]
                |> toClosedPath |> at (-0.02, -0.01) |> withFill Brushes.Black
        yield [lineTo (-0.016, -0.007)] |> toPath |> at (-0.019, -0.01) |> withZ -0.001 |> withPen { Pens.Black with Thickness = 0.003 }
        yield [lineTo (0.076, -0.007)] |> toPath |> at (0.019, -0.01) |> withZ -0.001 |> withPen { Pens.Black with Thickness = 0.003 }
    } 

    let mustach = shapes {
        yield [ bezierTo (0.03, 0.02) (0.02, 0.0) (0.0, -0.01)
                bezierTo (-0.03, -0.01) (-0.01, -0.01) (0.02, 0.0)
                bezierTo (-0.03, 0.01) (-0.02, 0.0) (0.01, -0.01)
                bezierTo (0.03, -0.02) (0.0, -0.01) (-0.02, 0.0) ]
                |> toClosedPath |> at origin |> withFill Brushes.Maroon
    }

    let skull skinColor = shapes {
        yield ellipse (0.14, 0.20) |> at origin |> withFill (Brush.FromColor skinColor)
    }

    let eye skinColor irisColor = {
        SkinColor = skinColor
        OuterPath = ellipse (0.03, 0.015)
        Cornea = shapes { yield ellipse (0.03, 0.015) |> at origin |> withFill Brushes.White }
        Iris = shapes { yield circle 0.006 |> at origin |> withFill (Brush.FromColor(irisColor)) } |> at origin
        Pupil = shapes { yield circle 0.002 |> at origin |> withFill Brushes.Black } |> at origin
    }

    let nose = shapes {
        yield [ bezierTo (0.0, 0.03) (-0.02, 0.03) (0.0, 0.0) ]
                |> toPath |> at (0.0, -0.015) |> withZ 0.01 |> withPen { Pens.Black with Thickness = 0.002 }
    }

    let mouth = {
        Lips = shapes {
            yield [ bezierTo (0.04, 0.0) (0.0, 0.0) (0.0, 0.0)
                    bezierTo (-0.04, 0.0) (-0.01, 0.01) (0.01, 0.01) ]
                    |> toClosedPath |> at (-0.02, 0.0) |> withZ 0.001 |> withFill Brushes.PaleVioletRed
        }
    }

