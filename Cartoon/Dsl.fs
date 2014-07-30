namespace FCartoon

[<ReflectedDefinition>]
module Dsl =

    let rectangle (width, height) = ClosedShape.Rectangle(Vector(width, height))
    let square (width) = ClosedShape.Rectangle(Vector(width, width))
    let ellipse (width, height) = ClosedShape.Ellipse(Vector(width, height))
    let circle (radius) = ClosedShape.Ellipse(Vector(radius * 2.0, radius * 2.0))
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

    let placedMap f (r, s) = r, f(s)

    let Pi = System.Math.PI

    open LazyList

    let transforms = lazylist

    let apply transform frames = 
        lazylist {
            for i in 0..frames-1 do
            yield { transform = transform; z = 0.0 }
        }

    let applyi f frames = 
        lazylist {
            for i in 0..frames-1 do
            yield { transform = f i; z = 0.0 }
        }

    let slideWithZ (x1, y1, z1) (x2, y2, z2) frames =
        lazylist {
            for i in 0..frames-1 do
            let ratio = float  i / float frames
            yield { transform = Transforms.translate ((x2 - x1) * ratio, (y2 - y1) * ratio); z = (z2 - z1) * ratio }
        }

    let slide (x1, y1) (x2, y2) = slideWithZ (x1, y1, 0.0) (x2, y2, 0.0)

    let framesOf generator frames =
        generator frames

    let followedBy t2 t1 = lazy (
        let t1' = LazyList.eval t1
        let t2' =
            match t1' |> LazyList.last with
            | Some (l:RefSpace) -> t2 |> LazyList.map ((+) l)
            | None -> t2
        LazyList.LazyConcat(t1', t2'))

    let Kappa = 0.5522848
