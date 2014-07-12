let colorType = System.Type.GetType("System.Drawing.Color")

let colors =
    typeof<System.Drawing.Color>.GetProperties(
        System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Public)
    |> Seq.map(fun m ->
        let color = m.GetMethod.Invoke(null, [||]) :?> System.Drawing.Color
        let toRelative x = (double x) / 255.0
        m.Name, (color.A |> toRelative, color.R |> toRelative, color.G |> toRelative, color.B |> toRelative), color.A <> 0uy)
    |> Seq.toList

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
