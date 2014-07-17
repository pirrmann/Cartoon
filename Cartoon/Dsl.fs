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
    let withContour pen (space, shape) = space, ClosedShape(shape, Contour(pen))
    let withFill brush (space, shape) = space, ClosedShape(shape, Fill(brush))
    let withContourAndFill (pen, brush) (space, shape) = space, ClosedShape(shape, ContourAndFill(pen, brush))
    let withPen pen (space, path) = space, Path(path, pen)
    let at (x, y) element = RefSpace.At(x, y), element
    let withZ z (refSpace, element) = { refSpace with z = z }, element
    let rotatedBy alpha (refSpace:RefSpace, element) = ({refSpace with transform = (Transforms.rotate alpha) * refSpace.transform}, element)
    let origin = (0.0, 0.0)

    let Pi = System.Math.PI