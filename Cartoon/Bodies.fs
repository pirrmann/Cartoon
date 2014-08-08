namespace FCartoon

open LazyList
open Builders
open Animations
open Dsl

[<ReflectedDefinition>]
type Eye =
    {
        SkinColor: Color
        OuterPath: ClosedShape
        Cornea: Shapes
        Iris: PlacedShapes
        Pupil: PlacedShapes
    }
    interface IAnimatable with
        member e.GetFrame(animations:AnimationFrames) = shapes {
            yield! e.Cornea |> at origin |> withZ 0.001
            yield! e.Iris |> withZ 0.002
            yield! e.Pupil |> withZ 0.003
            for animation in animations do
                match animation.Animation with
                | Animation.Blink _ ->
                    let ratio =
                        if animation.CurrentFrame < animation.FramesCount / 2
                        then 1.0 - (2.0 * float animation.CurrentFrame / float animation.FramesCount)
                        else (2.0 * float (animation.CurrentFrame + 1) / float animation.FramesCount) - 1.0
                    let hole = e.OuterPath |> at origin |> scaledByY ratio

                    yield e.OuterPath
                            |> withHole hole
                            |> at origin |> withZ 0.005 |> withFill { Color = e.SkinColor }
                    yield hole
                            |> withZ 0.006 |> withContour { Color = e.SkinColor; Thickness = 0.001 }
                | _ -> ()
            }

[<ReflectedDefinition>]
type Mouth =
    {
        Lips: Shapes
    }
    interface IAnimatable with
        member m.GetFrame(animations:AnimationFrames) = shapes {
            for animation in animations do
                match animation.Animation with
                | Animation.Smile ->
                    let ratio =
                        if animation.CurrentFrame < animation.FramesCount / 2
                        then 2.0 * float animation.CurrentFrame / float animation.FramesCount
                        else 2.0 - (2.0 * float (animation.CurrentFrame-1) / float animation.FramesCount)
                    yield! m.Lips |> at (0.0, 0.0) |> scaledByY (1.0 + ratio)
                | _ -> yield! m.Lips |> at origin }                

[<ReflectedDefinition>]
type Head =
    {
        SkinColor: Color
        Skull: Shapes
        Eyes: (RefSpace * Eye) * (RefSpace * Eye)
        Nose: RefSpace * Shapes
        Mouth: RefSpace * Mouth
        Accessories: PlacedShapes list
    }
    interface IAnimatable with
        member h.GetFrame(animations:AnimationFrames) = shapes {
            yield! h.Skull |> at origin
            yield! fst(h.Eyes) |> placedMap (fun e -> (e :> IAnimatable).GetFrame(animations |> onlyLeft))
            yield! snd(h.Eyes) |> placedMap (fun e -> (e :> IAnimatable).GetFrame(animations |> onlyRight))
            yield! h.Nose
            yield! h.Mouth |> placedMap (fun e -> (e :> IAnimatable).GetFrame(animations))
            for a in h.Accessories do yield! a
        }

[<ReflectedDefinition>]
type Human =
    {
        Head: RefSpace * Head
    }

[<ReflectedDefinition>]
module BodyDsl =
    let private script animation target startFrame framesCount  = { Animation = animation; Target = target; StartFrame = startFrame; FramesCount = framesCount }
    let blink = script Animation.Blink AnimationTarget.Global
    let blinkLeft = script Animation.Blink AnimationTarget.Left
    let blinkRight = script Animation.Blink AnimationTarget.Right
    let smile = script Animation.Smile AnimationTarget.Global