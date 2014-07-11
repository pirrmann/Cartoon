namespace FCartoon

[<ReflectedDefinition>]
module Colors =
   let Transparent = { Alpha = 0.000000; R = 1.000000; G = 1.000000; B = 1.000000 }
   let White = { Alpha = 1.000000; R = 1.000000; G = 1.000000; B = 1.000000 }
   let Black = { Alpha = 1.000000; R = 0.000000; G = 0.000000; B = 0.000000 }
   let Red = { Alpha = 1.000000; R = 1.000000; G = 0.000000; B = 0.000000 }
   let Green = { Alpha = 1.000000; R = 0.000000; G = 1.000000; B = 0.000000 }
   let Blue = { Alpha = 1.000000; R = 0.000000; G = 0.000000; B = 1.000000 }
   let Pink = { Alpha = 1.000000; R = 1.000000; G = 0.000000; B = 0.800000 }

[<ReflectedDefinition>]
module Pens =
   let White = { Color = Colors.White; Thickness = 1.0 }
   let Black = { Color = Colors.Black; Thickness = 1.0 }
   let Red = { Color = Colors.Red; Thickness = 1.0 }
   let Green = { Color = Colors.Green; Thickness = 1.0 }
   let Blue = { Color = Colors.Blue; Thickness = 1.0 }
   let Pink = { Color = Colors.Pink; Thickness = 1.0 }

[<ReflectedDefinition>]
module Brushes =
   let White = { Color = Colors.White }
   let Black = { Color = Colors.Black }
   let Red = { Color = Colors.Red }
   let Green = { Color = Colors.Green }
   let Blue = { Color = Colors.Blue }
   let Pink = { Color = Colors.Pink }
