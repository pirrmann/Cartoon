namespace FCartoon

[<ReflectedDefinition>]
module Colors =
    let Transparent = { Alpha = 0.0; R = 1.0; G = 1.0; B = 1.0 }
    let White =       { Alpha = 1.0; R = 1.0; G = 1.0; B = 1.0 }
    let Black =       { Alpha = 1.0; R = 0.0; G = 0.0; B = 0.0 }
    let Red =         { Alpha = 1.0; R = 1.0; G = 0.0; B = 0.0 }
    let Green =       { Alpha = 1.0; R = 0.0; G = 1.0; B = 0.0 }
    let Blue =        { Alpha = 1.0; R = 0.0; G = 0.0; B = 1.0 }

[<ReflectedDefinition>]
module Pens =
    let White = { Color = Colors.White; Thickness = 1.0 }
    let Black = { Color = Colors.Black; Thickness = 1.0 }
    let Red =   { Color = Colors.Red; Thickness = 1.0 }
    let Green = { Color = Colors.Green; Thickness = 1.0 }
    let Blue =  { Color = Colors.Blue; Thickness = 1.0 }

[<ReflectedDefinition>]
module Brushes =
    let White = { Color = Colors.White }
    let Black = { Color = Colors.Black }
    let Red =   { Color = Colors.Red }
    let Green = { Color = Colors.Green }
    let Blue =  { Color = Colors.Blue }
