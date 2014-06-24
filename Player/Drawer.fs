module Drawer

open Shapes
open Cartoon

let toSystemColor (color:Shapes.Color) =
    System.Drawing.Color.FromArgb(int(color.Alpha * 255.0), int(color.R * 255.0), int(color.G * 255.0), int(color.B * 255.0))

let toSystemPen (pen:Shapes.Pen) =
    new System.Drawing.Pen(pen.Color |> toSystemColor, pen.Thickness |> single)

let toSystemBrush (brush:Shapes.Brush) =
    new System.Drawing.SolidBrush(brush.Color |> toSystemColor)

open System.Drawing

let toSystemPoint (Vector(x, y)) = new System.Drawing.PointF(single x, single y)

let drawShape (graphics:Graphics) (space:RefSpace, shape) =
    match shape with
    | Rectangle(Vector(width, height), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawRectangle(pen, space.x |> float32, space.y |> float32, width |> float32, height |> float32)
    | RectangleFill(Vector(width, height), brush) ->
        use brush = brush |> toSystemBrush
        graphics.FillRectangle(brush, space.x |> float32, space.y |> float32, width |> float32, height |> float32)
    | Ellipse(Vector(width, height), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawEllipse(pen, space.x - width/2.0 |> float32, space.y - height/2.0 |> float32, width |> float32, height |> float32)
    | EllipseFill(Vector(width, height), brush) ->
        use brush = brush |> toSystemBrush
        graphics.FillEllipse(brush, space.x - width/2.0 |> float32, space.y - height/2.0 |> float32, width |> float32, height |> float32)
    | Line(Vector(x, y), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawLine(pen, space.x |> float32, space.y |> float32, space.x + x |> float32, space.y + y |> float32)
    | Bezier(v, t1, t2, pen) ->
        use pen = pen |> toSystemPen
        let p1 = Vector(space.x, space.y)
        let p2 = p1 + v
        graphics.DrawBezier(pen, p1 |> toSystemPoint, p1 + t1 |> toSystemPoint, p2 + t2 |> toSystemPoint, p2 |> toSystemPoint)

let draw (graphics:Graphics) (space, frame) =
    graphics.Clear(Color.White)
    for anchor, shape in frame |> Seq.sortBy (fun (s, _) -> s.z) do
    (space + anchor, shape) |> drawShape graphics

let play (graphics:Graphics) (clip:Clip) =
    match clip.GetFrame() with
    | Some(point, frame) ->
        use image = new Bitmap(640, 480, graphics)
        use g2 = Graphics.FromImage(image)
        draw g2 (point, frame)
        graphics.DrawImage(image, 0, 0)
    | None -> ()
