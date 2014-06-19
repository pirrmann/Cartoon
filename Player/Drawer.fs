module Drawer

open Shapes
open Cartoon

let toSystemColor (color:Shapes.Color) =
    System.Drawing.Color.FromArgb(int(color.Alpha * 255.0), int(color.R * 255.0), int(color.G * 255.0), int(color.B * 255.0))

open System.Drawing

let drawShape (graphics:Graphics) (point, shape) =
    match shape with
    | Rectangle(x, y, color) ->
        use brush = new SolidBrush(color |> toSystemColor)
        graphics.FillRectangle(brush, point.x, point.y, x, y)
    | Ellipse(x, y, color) ->
        use brush = new SolidBrush(color |> toSystemColor)
        graphics.FillEllipse(brush, point.x - x/2, point.y - y/2, x, y)

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
