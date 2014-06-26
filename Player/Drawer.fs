module Drawer

open FCartoon

let toSystemColor (color:Color) =
    System.Drawing.Color.FromArgb(int(color.Alpha * 255.0), int(color.R * 255.0), int(color.G * 255.0), int(color.B * 255.0))

let toSystemPen (pen:Pen) =
    new System.Drawing.Pen(pen.Color |> toSystemColor, pen.Thickness |> single)

let toSystemBrush (brush:Brush) =
    new System.Drawing.SolidBrush(brush.Color |> toSystemColor)

let toSystemTransform (TransformMatrix((m11, m12), (m21, m22), (dx, dy))) =
    new System.Drawing.Drawing2D.Matrix(float32 m11, float32 m12, float32 m21, float32 m22, float32 dx, float32 dy)

open System.Drawing

let toSystemPoint (Vector(x, y)) = new System.Drawing.PointF(single x, single y)

let drawShape (graphics:Graphics) (space:RefSpace, shape) =
    graphics.TranslateTransform(320.0f, 240.0f)
    graphics.MultiplyTransform(space.transform |> toSystemTransform)
    match shape with
    | Rectangle(Vector(width, height), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawRectangle(pen, 0.0f, 0.0f, width |> float32, height |> float32)
    | RectangleFill(Vector(width, height), brush) ->
        use brush = brush |> toSystemBrush
        graphics.FillRectangle(brush, 0.0f, 0.0f, width |> float32, height |> float32)
    | Ellipse(Vector(width, height), pen) ->
        graphics.MultiplyTransform(Transforms.translate (- width/2.0, - height/2.0) |> toSystemTransform)
        use pen = pen |> toSystemPen
        graphics.DrawEllipse(pen, 0.0f, 0.0f, width |> float32, height |> float32)
    | EllipseFill(Vector(width, height), brush) ->
        graphics.MultiplyTransform(Transforms.translate (- width/2.0, - height/2.0) |> toSystemTransform)
        use brush = brush |> toSystemBrush
        graphics.FillEllipse(brush, 0.0f, 0.0f, width |> float32, height |> float32)
    | Line(Vector(x, y), pen) ->
        use pen = pen |> toSystemPen
        graphics.DrawLine(pen, 0.0f, 0.0f, x |> float32, y |> float32)
    | Bezier(v, t1, t2, pen) ->
        use pen = pen |> toSystemPen
        let p2 = v
        graphics.DrawBezier(pen, Vector(0.0, 0.0) |> toSystemPoint,  t1 |> toSystemPoint, p2 + t2 |> toSystemPoint, p2 |> toSystemPoint)

    graphics.ResetTransform()

let draw (graphics:Graphics) (space, frame) =
    graphics.Clear(Color.White)
    for anchor, shape in frame |> Seq.sortBy (fun (s, _) -> s.z) do
    (space + anchor, shape) |> drawShape graphics

let play (graphics:Graphics) (clip:Clip) =
    match clip.GetFrame() with
    | Some(space, frame) ->
        use image = new Bitmap(640, 480, graphics)
        use g2 = Graphics.FromImage(image)
        draw g2 (space, frame)
        graphics.DrawImage(image, 0, 0)
    | None -> ()
