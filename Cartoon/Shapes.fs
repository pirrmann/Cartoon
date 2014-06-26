namespace FCartoon

open Microsoft.FSharp.Math

type Vector = Vector of x:float * y:float
    with static member Zero = Vector (0.0, 0.0)
            static member (+) (Vector(x1, y1) , Vector(x2, y2)) = Vector(x1 + x2, y1 + y2)
            static member (-) (Vector(x1, y1) , Vector(x2, y2)) = Vector(x1 - x2, y1 - y2)

type TransformMatrix =
    | TransformMatrix of (float * float) * (float * float) * (float * float)
    with member this.x = match this with | TransformMatrix(_,_,(x,_)) -> x
            member this.y = match this with | TransformMatrix(_,_,(_,y)) -> y
            static member (*) (x, y) =
                match x, y with
                | TransformMatrix((m11, m12), (m21, m22), (mx, my)), TransformMatrix((n11, n12), (n21, n22), (nx, ny)) ->
                  TransformMatrix((m11 * n11 + m12 * n21, m11 * n12 + m12 * n22),
                                  (m21 * n11 + m22 * n21, m21 * n12 + m22 * n22),
                                  (mx * n11 + my * n21 + nx, mx * n12 + my * n22 + ny))

module Transforms =
    let rotate alpha = TransformMatrix((cos alpha, sin alpha), (-sin alpha, cos alpha), (0.0, 0.0))
    let translate (x, y) = TransformMatrix((1.0, 0.0), (0.0, 1.0), (x, y))
    let scale ratio = TransformMatrix((ratio, 0.0), (0.0, ratio), (0.0, 0.0))

type RefSpace = { transform:TransformMatrix; z:float } with
    static member Origin = { transform = Transforms.translate (0.0, 0.0); z = 0.0 }
    static member At(x, y) = { transform = Transforms.translate (x, y); z = 0.0 }
    static member Transform(transform) = { transform = transform; z = 0.0 }
    static member (+) (s1, s2) = { transform = s2.transform * s1.transform; z = s1.z + s2.z }
    member this.x = this.transform.x
    member this.y = this.transform.y

type Color = { Alpha:float; R: float; G: float; B: float} with
    static member Transparent = { Alpha = 0.0; R = 1.0; G = 1.0; B = 1.0 }
    static member White =       { Alpha = 1.0; R = 1.0; G = 1.0; B = 1.0 }
    static member Black =       { Alpha = 1.0; R = 0.0; G = 0.0; B = 0.0 }
    static member Red =         { Alpha = 1.0; R = 1.0; G = 0.0; B = 0.0 }
    static member Green =       { Alpha = 1.0; R = 0.0; G = 1.0; B = 0.0 }
    static member Blue =        { Alpha = 1.0; R = 0.0; G = 0.0; B = 1.0 }

type Pen = { Color:Color; Thickness:int } with
    static member White = { Color = Color.White; Thickness = 1 }
    static member Black = { Color = Color.Black; Thickness = 1 }
    static member Red =   { Color = Color.Red; Thickness = 1 }
    static member Green = { Color = Color.Green; Thickness = 1 }
    static member Blue =  { Color = Color.Blue; Thickness = 1 }

type Brush = { Color:Color } with
    static member White = { Color = Color.White }
    static member Black = { Color = Color.Black }
    static member Red =   { Color = Color.Red }
    static member Green = { Color = Color.Green }
    static member Blue =  { Color = Color.Blue }

type Shape =
    | RectangleFill of Size:Vector * Brush:Brush
    | Rectangle of Size:Vector * Pen:Pen
    | EllipseFill of Size:Vector * Brush:Brush
    | Ellipse of Size:Vector * Pen:Pen
    | Line of Vector:Vector * Pen:Pen
    | Bezier of Vector:Vector * tangent1:Vector * tangent2:Vector * Pen:Pen
and Shapes = (RefSpace * Shape) list