module Shapes

type Vector = Vector of x:float * y:float
    with static member Zero = Vector (0.0, 0.0)
         static member (+) (Vector(x1, y1) , Vector(x2, y2)) = Vector(x1 + x2, y1 + y2)
         static member (-) (Vector(x1, y1) , Vector(x2, y2)) = Vector(x1 - x2, y1 - y2)

type RefSpace = { x:float; y:float; z:float; angle:float; scale: float }
    with static member Origin = { x = 0.0; y = 0.0; z = 0.0; angle = 0.0; scale = 1.0 }
         static member At(x, y) = { x = x; y = y; z = 0.0; angle = 0.0; scale = 1.0 }
         static member (+) (s1, s2) = { x = s1.x + s2.x
                                        y = s1.y + s2.y
                                        z = s1.z + s2.z
                                        angle = 0.0
                                        scale = 1.0 }
         static member (-) (s1, s2) = { x = s1.x - s2.x
                                        y = s1.y - s2.y
                                        z = s1.z + s2.z
                                        angle = 0.0
                                        scale = 1.0 }

type Color = { Alpha:float; R: float; G: float; B: float}
             with static member Transparent = { Alpha = 0.0; R = 1.0; G = 1.0; B = 1.0 }
                  static member White =       { Alpha = 1.0; R = 1.0; G = 1.0; B = 1.0 }
                  static member Black =       { Alpha = 1.0; R = 0.0; G = 0.0; B = 0.0 }
                  static member Red =         { Alpha = 1.0; R = 1.0; G = 0.0; B = 0.0 }
                  static member Green =       { Alpha = 1.0; R = 0.0; G = 1.0; B = 0.0 }
                  static member Blue =        { Alpha = 1.0; R = 0.0; G = 0.0; B = 1.0 }

type Pen = { Color:Color; Thickness:int }
             with static member White = { Color = Color.White; Thickness = 1 }
                  static member Black = { Color = Color.Black; Thickness = 1 }
                  static member Red =   { Color = Color.Red; Thickness = 1 }
                  static member Green = { Color = Color.Green; Thickness = 1 }
                  static member Blue =  { Color = Color.Blue; Thickness = 1 }

type Brush = { Color:Color }
             with static member White = { Color = Color.White }
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

let combinePlacedElements (s1, f1) (s2, f2) =
    let diff = s2 - s1
    s1, f1 @ (f2 |> List.map (fun (s:RefSpace, e) -> s + diff, e))
