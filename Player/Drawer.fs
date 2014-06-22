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

let toSystemPoint (Vector(x, y)) = new System.Drawing.Point(x, y)

let drawShape (graphics:Graphics) (point:Shapes.Point, shape) =
    match shape with
    | Rectangle(Vector(width, height), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawRectangle(pen, point.x, point.y, width, height)
    | RectangleFill(Vector(width, height), brush) ->
        use brush = brush |> toSystemBrush
        graphics.FillRectangle(brush, point.x, point.y, width, height)
    | Ellipse(Vector(width, height), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawEllipse(pen, point.x - width/2, point.y - height/2, width, height)
    | EllipseFill(Vector(width, height), brush) ->
        use brush = brush |> toSystemBrush
        graphics.FillEllipse(brush, point.x - width/2, point.y - height/2, width, height)
    | Line(Vector(x, y), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawLine(pen, point.x, point.y, point.x + x, point.y + y)
    | Bezier(v, t1, t2, pen) ->
        use pen = pen |> toSystemPen
        let p1 = Vector(point.x, point.y)
        let p2 = p1 + v
        graphics.DrawBezier(pen, p1 |> toSystemPoint, p1 + t1 |> toSystemPoint, p2 + t2 |> toSystemPoint, p2 |> toSystemPoint)

let draw (graphics:Graphics) (point, frame) =
    graphics.Clear(Color.White)
    for anchor, shape in frame |> Seq.sortBy (fun (p, _) -> p.z) do
    (point + anchor, shape) |> drawShape graphics

let play (graphics:Graphics) (clip:Clip) =
    match clip.GetFrame() with
    | Some(point, frame) ->
        use image = new Bitmap(640, 480, graphics)
        use g2 = Graphics.FromImage(image)
        draw g2 (point, frame)
        graphics.DrawImage(image, 0, 0)
    | None -> ()
