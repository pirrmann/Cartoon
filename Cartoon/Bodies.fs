namespace FCartoon

open Builders
open Dsl

[<ReflectedDefinition>]
type Eye =
    {
        Cornea: Shapes
        Iris: PlacedShapes
        Pupil: PlacedShapes
    } with
    member e.ToClip() = clips {
            yield e.Cornea |> at origin |> withZ 0.001
            yield e.Iris |> withZ 0.002
            yield e.Pupil |> withZ 0.003
        }
    static member toClip = fun (e:Eye) -> e.ToClip()

[<ReflectedDefinition>]
type Head =
    {
        Skull: Clip
        Eyes: (RefSpace * Eye) * (RefSpace * Eye)
        Nose: RefSpace * Clip
        Mouth: RefSpace * Clip
        Accessories: PlacedShapes list
    } with
    member h.ToClip() = clips {
            yield! h.Skull |> at origin
            yield! fst(h.Eyes) |> placedMap Eye.toClip
            yield! snd(h.Eyes) |> placedMap Eye.toClip
            yield! h.Nose
            yield! h.Mouth
            for a in h.Accessories do yield a
        }

[<ReflectedDefinition>]
type Human =
    {
        Head: RefSpace * Head
    }