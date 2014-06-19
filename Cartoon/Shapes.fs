module Shapes

open System.Drawing

type Point = { x:int; y:int; z:int }
    with static member Origin = { x=0; y=0; z=0 }
         static member (+) (p1, p2) = { x = p1.x + p2.x
                                        y = p1.y + p2.y
                                        z = p1.z + p2.z }
         static member (-) (p1, p2) = { x = p1.x - p2.x
                                        y = p1.y - p2.y
                                        z = p1.z - p2.z }

type Shape =
    | Rectangle of int * int * Color
    | Ellipse of int * int * Color
and Shapes = (Point * Shape) list

let combinePlacedElements (p1, f1) (p2, f2) =
    let diff = p2 - p1
    p1, f1 @ (f2 |> List.map (fun (p:Point, s) -> p + diff, s))

let drawShape (graphics:Graphics) (point, shape) =
    match shape with
    | Rectangle(x, y, color) ->
        use brush = new SolidBrush(color)
        graphics.FillRectangle(brush, point.x, point.y, x, y)
    | Ellipse(x, y, color) ->
        use brush = new SolidBrush(color)
        graphics.FillEllipse(brush, point.x - x/2, point.y - y/2, x, y)

let draw (graphics:Graphics) (point, frame) =
    graphics.Clear(Color.White)
    for anchor, shape in frame |> Seq.sortBy (fun (p, _) -> p.z) do
    (point + anchor, shape) |> drawShape graphics