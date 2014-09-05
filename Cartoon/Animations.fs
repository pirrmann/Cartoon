namespace FCartoon

[<ReflectedDefinition>]
module Animations =

    [<RequireQualifiedAccess>]
    type AnimationTarget =
        | Left
        | Right
        | Global

    [<RequireQualifiedAccess>]
    type Animation =
        | None
        | Blink
        | Smile
        | Talk of int

    type ScriptedAnimation =
        {
            Animation: Animation
            Target: AnimationTarget
            StartFrame: int
            FramesCount: int
        }

    type AnimationFrame =
        {
            Animation: Animation
            Target: AnimationTarget
            CurrentFrame: int
            FramesCount: int
        }

    type AnimationFrames = AnimationFrame list

    let onlyLeft = List.filter (fun (f:AnimationFrame) -> f.Target = AnimationTarget.Left || f.Target = AnimationTarget.Global)
    let onlyRight = List.filter (fun (f:AnimationFrame) -> f.Target = AnimationTarget.Right || f.Target = AnimationTarget.Global)
