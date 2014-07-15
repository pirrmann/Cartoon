[<ReflectedDefinition>]
module Program

open FCartoon
open FunScript
open FunScript.TypeScript

let toCanvasTransform (TransformMatrix((m11, m12), (m21, m22), (dx, dy)))= m11, m12, m21, m22, dx, dy
let toCanvasColor (color:Color) =
    let to255color (c:float) =
        let i = (c * 255.0) |> System.Math.Round
        i.ToString()
    "rgba("
    + ([color.R; color.G; color.B; color.Alpha] |> List.map to255color |> String.concat ",")
    + ")"

let toPoint (Vector(x, y)) = x, y

type CanvasPathPart =
    | MoveTo of float * float
    | LineTo of float * float
    | BezierTo of (float * float) * (float * float) * (float * float)
    with member x.End = match x with
                        | MoveTo (x, y)
                        | LineTo (x, y)
                        | BezierTo (_, _, (x, y)) -> (x, y)

let buildPath path =
    let offset = ref Vector.Zero

    let rec buildPath' path = seq {
        for segment in path do
        match segment with
        | Line v ->
            offset := !offset + v
            yield LineTo (!offset |> toPoint)
        | Bezier (v, t1, t2) ->
            let x, y = !offset |> toPoint
            offset := !offset + v
            let x2, y2 = !offset |> toPoint
            let t1x, t1y = t1 |> toPoint
            let t2x, t2y = t2 |> toPoint
            yield BezierTo ((x + t1x, y + t1y), (x2 + t2x, y2 + t2y), (x2, y2))
        | CompositePath (path) ->
            yield! buildPath' path }

    seq {
        yield MoveTo (0.0, 0.0)
        yield! buildPath' [path] }

let rec getOuterPath shape = seq {
    match shape with
    | ClosedPath p ->
        yield! buildPath p
    | Rectangle(Vector(width, height)) ->
        yield MoveTo (0.0, 0.0)
        yield LineTo (width, 0.0)
        yield LineTo (width, height)
        yield LineTo (0.0, height)
        yield LineTo (0.0, 0.0)
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

        yield MoveTo (x, ym)
        yield BezierTo ((x, ym - oy), (xm - ox, y), (xm, y))
        yield BezierTo ((xm + ox, y), (xe, ym - oy), (xe, ym))
        yield BezierTo ((xe, ym + oy), (xm + ox, ye), (xm, ye))
        yield BezierTo ((xm - ox, ye), (x, ym + oy), (x, ym))
    | HollowShape(s1, _, _) -> yield! getOuterPath s1 }

let reversePath (pathParts:CanvasPathPart seq) =
    let reversedParts = (MoveTo (0.0, 0.0)) :: (pathParts |> List.ofSeq |> List.rev)

    let rec reverseMap parts = seq {
        match parts with
        | MoveTo _ :: next :: tail ->
            yield MoveTo (next.End)
            yield! reverseMap (next :: tail)
        | LineTo _ :: next :: tail ->
            yield LineTo (next.End)
            yield! reverseMap (next :: tail)
        | BezierTo ((cx1, cy1), (cx2, cy2), _) :: next :: tail ->
            yield BezierTo ((cx2, cy2), (cx1, cy1), (next.End))
            yield! reverseMap (next :: tail)
        | _ -> ()
    }

    reverseMap reversedParts

let translate (Vector(dx, dy)) = function
    | MoveTo (x, y) -> MoveTo (x + dx, y + dy)
    | LineTo (x, y) -> LineTo (x + dx, y + dy)
    | BezierTo ((cx1, cy1), (cx2, cy2), (x, y)) -> BezierTo ((cx2 + dx, cy2 + dy), (cx1 + dx, cy1 + dy), (x + dx, y + dy))

let rec getFillPath shape = seq {
    match shape with
    | HollowShape(s1, offset, s2) ->
        yield! getFillPath s1
        yield getOuterPath s2 |> reversePath |> Seq.map (translate offset)
    | _ -> yield getOuterPath shape }

let walk (ctx:CanvasRenderingContext2D) path =
    for pathPart in path do
    match pathPart with
    | MoveTo (x, y) -> ctx.moveTo(x, y)
    | LineTo (x, y) -> ctx.lineTo(x, y)
    | BezierTo ((cx1, cy1), (cx2, cy2), (x, y)) -> ctx.bezierCurveTo(cx1, cy1, cx2, cy2, x, y)

let drawShape (ctx:CanvasRenderingContext2D) (space:RefSpace, shape:Shape) =
    ctx.setTransform(space.transform * (Transforms.translate(320.0, 240.0)) |> toCanvasTransform)
    match shape with
    | ClosedShape(shape, drawType) ->
        drawType.Brush |> Option.iter (fun brush ->
            ctx.save()
            ctx.fillStyle <- brush.Color |> toCanvasColor
            ctx.beginPath()
            for path in shape |> getFillPath do
                path |> walk ctx 
                ctx.closePath()
            ctx.fill()
            ctx.restore())
        drawType.Pen |> Option.iter (fun pen ->
            ctx.save()
            ctx.fillStyle <- pen.Color |> toCanvasColor
            ctx.strokeStyle <- pen.Color |> toCanvasColor
            ctx.lineWidth <- pen.Thickness
            ctx.beginPath()
            shape |> getOuterPath |> walk ctx 
            ctx.closePath()
            ctx.stroke()
            ctx.restore())
    | Path(path, pen) ->
        ctx.save()
        ctx.fillStyle <- pen.Color |> toCanvasColor
        ctx.strokeStyle <- pen.Color |> toCanvasColor
        ctx.lineWidth <- pen.Thickness
        ctx.beginPath()
        buildPath path |> walk ctx 
        ctx.stroke()
        ctx.restore()

let draw (ctx:CanvasRenderingContext2D) (space, frame) =
    ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    ctx.fillStyle <- "rgb(255,255,255)"
    ctx.fillRect (0.0, 0.0, 640.0, 480.0)
    ctx.strokeStyle <- "rgb(0,0,0)"
    ctx.lineWidth <- 1.0
    ctx.strokeRect (0.0, 0.0, 640.0, 480.0)
    for anchor, shape in frame |> Seq.sortBy (fun (s, _) -> s.z) do
    (space + anchor, shape) |> drawShape ctx

let main () =
    let cartoon = SampleClips.test6

    let canvas = Globals.document.getElementsByTagName_canvas().[0]
    canvas.width <- 640.
    canvas.height <- 480.
    let ctx = canvas.getContext_2d()

    let startPlaying () = Player.play (draw ctx) cartoon |> Async.StartImmediate

    let restartButton = Globals.document.getElementsByTagName_input().[0]
    restartButton.onclick <- System.Func<MouseEvent, obj>(fun e -> startPlaying(); null)

    startPlaying()

do Runtime.Run(directory="Web")