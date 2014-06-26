namespace FCartoon

module Dsl =

    let rectangle (width, height) = ClosedShape.Rectangle(Vector(width, height))
    let square (width) = ClosedShape.Rectangle(Vector(width, width))
    let ellipse (width, height) = ClosedShape.Ellipse(Vector(width, width))
    let circle (radius) = ClosedShape.Ellipse(Vector(radius, radius))
    let line (x1, y1) (x2, y2) = RefSpace.At(x1, y1), Line(Vector(x2 - x1, y2 - y1))
    let bezier (x1, y1) (x2, y2) (tx1, ty1) (tx2, ty2) = RefSpace.At(x1, y1), Bezier(Vector(x2 - x1, y2 - y1), Vector(tx1, ty1), Vector(tx2, ty2))
    let withContour pen shape = ClosedShape(shape, Contour(pen))
    let withFill brush shape = ClosedShape(shape, Fill(brush))
    let withContourAndFill (pen, brush) shape = ClosedShape(shape, ContourAndFill(pen, brush))
    let withPen pen (space, path) = space, Path(path, pen)
    let at (x, y) (shape:Shape) = RefSpace.At(x, y), shape
    let origin = (0.0, 0.0)