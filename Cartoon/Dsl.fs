namespace FCartoon

[<ReflectedDefinition>]
module Dsl =

    let rectangle (width, height) = ClosedShape.Rectangle(Vector(width, height))
    let square (width) = ClosedShape.Rectangle(Vector(width, width))
    let ellipse (width, height) = ClosedShape.Ellipse(Vector(width, height))
    let circle (radius) = ClosedShape.Ellipse(Vector(radius, radius))
    let line (x1, y1) (x2, y2) = RefSpace.At(x1, y1), Line(Vector(x2 - x1, y2 - y1))
    let bezier (x1, y1) (x2, y2) (tx1, ty1) (tx2, ty2) = RefSpace.At(x1, y1), Bezier(Vector(x2 - x1, y2 - y1), Vector(tx1, ty1), Vector(tx2, ty2))
    let lineTo (x, y) = Line(Vector(x, y))
    let bezierTo (x, y) (tx1, ty1) (tx2, ty2) = Bezier(Vector(x, y), Vector(tx1, ty1), Vector(tx2, ty2))
    let toPath = CompositePath
    let toClosedPath = CompositePath >> ClosedPath
    let withHole hole shape = HollowShape(shape, hole)

    let resource = Resource
    let image (width, height) resource = Image(Vector(width, height), resource)

    let withContour pen (space, shape) = space, ClosedShape(shape, Contour(pen))
    let withFill brush (space, shape) = space, ClosedShape(shape, Fill(brush))
    let withContourAndFill (pen, brush) (space, shape) = space, ClosedShape(shape, ContourAndFill(pen, brush))
    let withPen pen (space, path) = space, Path(path, pen)

    let transform matrix refSpace = RefSpace.Transform(matrix) + refSpace

    let at (x, y) element = RefSpace.At(x, y), element
    let withZ z (refSpace, element) = { refSpace with z = z }, element
    let rotatedBy alpha (refSpace:RefSpace, element) = ({refSpace with transform = (Transforms.rotate alpha) * refSpace.transform}, element)
    let scaledBy ratio (refSpace:RefSpace, element) = ({refSpace with transform = (Transforms.scale ratio) * refSpace.transform}, element)
    let xFlipped (refSpace:RefSpace, element) = ({refSpace with transform = (Transforms.scaleX -1.0) * refSpace.transform}, element)
    let yFlipped (refSpace:RefSpace, element) = ({refSpace with transform = (Transforms.scaleY -1.0) * refSpace.transform}, element)
    let origin = (0.0, 0.0)

    let Pi = System.Math.PI

    open LazyList

    let slideWithZ (x, y, z) frames =
        lazylist {
            for i in 0..frames-1 do
            let ratio = float  i / float frames
            yield { transform = Transforms.translate (x * ratio, y * ratio); z = z * ratio }
        }

    let slide (x, y) = slideWithZ (x, y, 0.0)

    let whileApplying t = LazyList.map (transform t)

    let framesOf generator frames =
        generator frames

    let followedBy t2 t1 = lazy (
        let t1' = LazyList.eval t1
        let t2' =
            match t1' |> LazyList.last with
            | Some (l:RefSpace) -> t2 |> LazyList.map ((+) l)
            | None -> t2
        LazyList.LazyConcat(t1', t2'))
