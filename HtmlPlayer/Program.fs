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
            let x = -width / 2.0
            let y = -height / 2.0
            let kappa = 0.5522848
            let ox = (width / 2.0) * kappa  // control point offset horizontal
            let oy = (height / 2.0) * kappa // control point offset vertical
            let xe = x + width              // x-end
            let ye = y + height             // y-end
            let xm = x + width / 2.0        // x-middle
            let ym = y + height / 2.0       // y-middle

            ctx.beginPath()
            ctx.moveTo(x, ym);
            ctx.bezierCurveTo(x, ym - oy, xm - ox, y, xm, y);
            ctx.bezierCurveTo(xm + ox, y, xe, ym - oy, xe, ym);
            ctx.bezierCurveTo(xe, ym + oy, xm + ox, ye, xm, ye);
            ctx.bezierCurveTo(xm - ox, ye, x, ym + oy, x, ym);
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
    ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    ctx.fillStyle <- "rgb(255,255,255)"
    ctx.fillRect (0.0, 0.0, 640.0, 480.0);
    ctx.strokeStyle <- "rgb(0,0,0)"
    ctx.lineWidth <- 1.0
    ctx.strokeRect (0.0, 0.0, 640.0, 480.0);
    for anchor, shape in frame |> Seq.sortBy (fun (s, _) -> s.z) do
    (space + anchor, shape) |> drawShape ctx

let main () =
    let cartoon = SampleClips.test4

    let canvas = Globals.document.getElementsByTagName_canvas().[0]
    canvas.width <- 640.
    canvas.height <- 480.
    let ctx = canvas.getContext_2d()

    let startPlaying () = Player.play (draw ctx) cartoon |> Async.StartImmediate

    let restartButton = Globals.document.getElementsByTagName_input().[0]
    restartButton.onclick <- System.Func<MouseEvent, obj>(fun e -> startPlaying(); null)

    startPlaying()

do Runtime.Run(directory="Web")