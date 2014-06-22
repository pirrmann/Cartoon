module Shapes

type Point = { x:int; y:int; z:int }
    with static member Origin = { x = 0; y = 0; z = 0 }
         static member New(x, y, z) = { x = x; y = y; z = z }
         static member (+) (p1, p2) = { x = p1.x + p2.x
                                        y = p1.y + p2.y
                                        z = p1.z + p2.z }
         static member (-) (p1, p2) = { x = p1.x - p2.x
                                        y = p1.y - p2.y
                                        z = p1.z + p2.z }

type Vector = Vector of x:int * y:int
    with static member Zero = Vector (0, 0)
         static member (+) (Vector(x1, y1) , Vector(x2, y2)) = Vector(x1 + x2, y1 + y2)
         static member (-) (Vector(x1, y1) , Vector(x2, y2)) = Vector(x1 - x2, y1 - y2)
//                member this.X = match this with | Vector(x, _) -> x
//                member this.Y = match this with | Vector(_, y) -> y

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
and Shapes = (Point * Shape) list

let combinePlacedElements (p1, f1) (p2, f2) =
    let diff = p2 - p1
    p1, f1 @ (f2 |> List.map (fun (p:Point, s) -> p + diff, s))

