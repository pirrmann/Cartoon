[<ReflectedDefinition>]
module Program

open FCartoon
open FunScript
open FunScript.TypeScript

let toCanvasTransform (TransformMatrix((m11, m12), (m21, m22), (dx, dy)))= m11, m12, m21, m22, dx, dy
let toCanvasColor (color:Color) =
    let to255color (c:float) = (int (c * 255.0)).ToString()
    "rgba("
    + ([color.R; color.G; color.B; color.Alpha] |> List.map to255color |> String.concat ",")
    + ")"

let drawShape (ctx:CanvasRenderingContext2D) (space:RefSpace, shape:Shape) =
    ctx.setTransform(space.transform * (Transforms.translate(320.0, 240.0)) |> toCanvasTransform)
    match shape with
    | ClosedShape(shape, drawType) ->
        match shape with
        | Rectangle(Vector(width, height)) ->
            drawType.Brush |> Option.iter (fun brush ->
                ctx.fillStyle <- brush.Color |> toCanvasColor
                ctx.fillRect(0.0, 0.0, width, height))
            drawType.Pen |> Option.iter (fun pen ->
                ctx.fillStyle <- pen.Color |> toCanvasColor
                ctx.strokeStyle <- pen.Color |> toCanvasColor
                ctx.lineWidth <- pen.Thickness
                ctx.strokeRect(0.0, 0.0, width, height))
        | Ellipse(Vector(width, height)) ->
            ctx.beginPath()
            ctx.arc(0.0, 0.0, width / 2.0, 0.0, 2.0 * System.Math.PI)
            ctx.closePath()
            drawType.Brush |> Option.iter (fun brush ->
                ctx.fillStyle <- brush.Color |> toCanvasColor
                ctx.fill())
            drawType.Pen |> Option.iter (fun pen ->
                ctx.fillStyle <- pen.Color |> toCanvasColor
                ctx.strokeStyle <- pen.Color |> toCanvasColor
                ctx.lineWidth <- pen.Thickness
                ctx.stroke())
    | Path(path, pen) ->
        ctx.fillStyle <- pen.Color |> toCanvasColor
        ctx.strokeStyle <- pen.Color |> toCanvasColor
        ctx.lineWidth <- pen.Thickness
        match path with
        | Line(Vector(x, y)) ->
            ctx.beginPath()
            ctx.moveTo(0.0, 0.0)
            ctx.lineTo(x, y)
            ctx.stroke()
        | Bezier(Vector(x, y), Vector(t1x, t1y), Vector(t2x, t2y)) ->
            ctx.beginPath()
            ctx.moveTo(0.0, 0.0)
            ctx.bezierCurveTo(t1x, t1y, x + t2x, y + t2y, x, y)
            ctx.stroke()

let draw (ctx:CanvasRenderingContext2D) (space, frame) =
    ctx.clearRect(0.0, 0.0, 640.0, 480.0)
    for anchor, shape in frame |> Seq.sortBy (fun (s, _) -> s.z) do
    (space + anchor, shape) |> drawShape ctx

let play (ctx:CanvasRenderingContext2D) (clip:Clip) =
    match clip.GetFrame() with
    | Some(space, frame) ->
        draw ctx (space, frame)
    | None -> ()

open Builders
open Dsl

let testScene = shapes {
    yield rectangle (100.0, 100.0) |> withContour Pen.Blue |> at origin
    yield ellipse (100.0, 100.0) |> withContourAndFill ({Pen.Black with Thickness = 5.0}, Brush.Red) |> at (-50.0, -50.0)
    yield line (0.0, 0.0) (100.0, 100.0) |> withPen Pen.Green
    yield bezier (0.0, 0.0) (100.0, 100.0) (0.0, 50.0) (0.0, -50.0) |> withPen { Pen.Red with Thickness = 3.0 }
    }

let testClip = Frame(RefSpace.Origin, testScene)

let main () =
   let canvas = Globals.document.getElementsByTagName_canvas().[0]
   canvas.width <- 640.
   canvas.height <- 480.
   let ctx = canvas.getContext_2d()
   play ctx testClip

do Runtime.Run(directory="Web")