namespace FCartoon

open LazyList
open Builders
open Animations
open Dsl
open Curves

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
                    let progress = float animation.CurrentFrame / float animation.FramesCount
                    let ratio = progress |> curve DownUp
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
        Smile: float -> Shapes
    }
    interface IAnimatable with
        member m.GetFrame(animations:AnimationFrames) = shapes {
            match animations |> Seq.tryPick (fun a -> match a.Animation with | Animation.Smile | Animation.Talk _ -> Some a | _ -> None) with
            | Some animation ->
                match animation.Animation with
                | Animation.Smile ->
                    let progress = float animation.CurrentFrame / float animation.FramesCount
                    let ratio = progress |> curve (UpFlatDown 0.8)
                    yield! m.Smile ratio |> at origin
                | Animation.Talk syllables ->
                    let scaleRatios = [|0.3; 0.5; 0.3; 0.5; 0.2; 0.5; 0.2; 0.4|]
                    let framesPerSyllable = animation.FramesCount / syllables
                    let syllableIndex = animation.CurrentFrame / framesPerSyllable
                    let frameInSyllable = animation.CurrentFrame % framesPerSyllable
                    let progressInSyllalbe = float frameInSyllable / float framesPerSyllable
                    let ratio = progressInSyllalbe |> curve UpDown
                    yield! m.Smile 0.0 |> at (0.0, 0.0) |> scaledByY (1.0 + ratio * scaleRatios.[syllableIndex % scaleRatios.Length])
                | _ -> ()                
            | _ -> yield! m.Smile 0.0 |> at origin }                

[<ReflectedDefinition>]
type Head =
    {
        SkinColor: Color
        Skull: Shapes
        Eyes: (RefSpace * Eye) * (RefSpace * Eye)
        Nose: PlacedShapes
        Mouth: RefSpace * Mouth
        StaticAccessories: PlacedShapes list
        ActiveAccessories: (RefSpace * IAnimatable) list
    }
    interface IAnimatable with
        member h.GetFrame(animations:AnimationFrames) = shapes {
            yield! h.Skull |> at origin
            yield! fst(h.Eyes) |> placedMap (fun e -> (e :> IAnimatable).GetFrame(animations |> onlyLeft))
            yield! snd(h.Eyes) |> placedMap (fun e -> (e :> IAnimatable).GetFrame(animations |> onlyRight))
            yield! h.Nose
            yield! h.Mouth |> placedMap (fun m -> (m :> IAnimatable).GetFrame(animations))
            for a in h.StaticAccessories do yield! a
            for a in h.ActiveAccessories do yield! a |> placedMap (fun e -> e.GetFrame(animations))
        }

[<ReflectedDefinition>]
type Human =
    {
        Head: RefSpace * Head
        Torso: PlacedShapes
        Legs: PlacedShapes
    }
    interface IAnimatable with
        member h.GetFrame(animations:AnimationFrames) = shapes {
            yield! h.Head |> placedMap (fun e -> (e :> IAnimatable).GetFrame(animations))
            yield! h.Torso
            yield! h.Legs
        }

[<ReflectedDefinition>]
module BodyDsl =
    let private script animation target startFrame framesCount  = { Animation = animation; Target = target; StartFrame = startFrame; FramesCount = framesCount }
    let blink = script Animation.Blink AnimationTarget.Global
    let blinkLeft = script Animation.Blink AnimationTarget.Left
    let blinkRight = script Animation.Blink AnimationTarget.Right
    let smile = script Animation.Smile AnimationTarget.Global
    let talk syllables = script (Animation.Talk syllables) AnimationTarget.Global