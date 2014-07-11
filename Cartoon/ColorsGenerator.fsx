let colors =
    [
    "Transparent", (0.0, 1.0, 1.0, 1.0), false
    "White", (1.0, 1.0, 1.0, 1.0), true
    "Black", (1.0, 0.0, 0.0, 0.0), true
    "Red", (1.0, 1.0, 0.0, 0.0), true
    "Green", (1.0, 0.0, 1.0, 0.0), true
    "Blue", (1.0, 0.0, 0.0, 1.0), true
    "Pink", (1.0, 1.0, 0.0, 0.8), true
    ]

let genLines = seq {
    yield "namespace FCartoon"
    yield ""
    yield "[<ReflectedDefinition>]"
    yield "module Colors ="
    for (name, (a, r, g, b), _) in colors do
        yield sprintf "   let %s = { Alpha = %f; R = %f; G = %f; B = %f }" name a r g b
    yield ""
    yield "[<ReflectedDefinition>]"
    yield "module Pens ="
    for (name, _, gen) in colors do
        if gen then
            yield sprintf "   let %s = { Color = Colors.%s; Thickness = 1.0 }" name name
    yield ""
    yield "[<ReflectedDefinition>]"
    yield "module Brushes ="
    for (name, _, gen) in colors do
        if gen then
            yield sprintf "   let %s = { Color = Colors.%s }" name name
}

System.IO.File.WriteAllLines(
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Colors.fs"),
    genLines |> Seq.toArray)
