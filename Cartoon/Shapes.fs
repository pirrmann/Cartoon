module Shapes

type Point = { x:int; y:int; z:int }
    with static member Origin = { x=0; y=0; z=0 }
         static member (+) (p1, p2) = { x = p1.x + p2.x
                                        y = p1.y + p2.y
                                        z = p1.z + p2.z }
         static member (-) (p1, p2) = { x = p1.x - p2.x
                                        y = p1.y - p2.y
                                        z = p1.z - p2.z }

type Color = { Alpha:float; R: float; G: float; B: float}
             with static member Transparent = { Alpha = 0.0; R = 1.0; G = 1.0; B = 1.0 }
                  static member White =       { Alpha = 1.0; R = 1.0; G = 1.0; B = 1.0 }
                  static member Black =       { Alpha = 1.0; R = 0.0; G = 0.0; B = 0.0 }
                  static member Red =         { Alpha = 1.0; R = 1.0; G = 0.0; B = 0.0 }
                  static member Green =       { Alpha = 1.0; R = 0.0; G = 1.0; B = 0.0 }
                  static member Blue =        { Alpha = 1.0; R = 0.0; G = 0.0; B = 1.0 }

type Shape =
    | Rectangle of int * int * Color
    | Ellipse of int * int * Color
and Shapes = (Point * Shape) list

let combinePlacedElements (p1, f1) (p2, f2) =
    let diff = p2 - p1
    p1, f1 @ (f2 |> List.map (fun (p:Point, s) -> p + diff, s))

