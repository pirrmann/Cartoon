namespace FCartoon

[<ReflectedDefinition>]
module Builders =
    let rec private combineClips c1 c2 =
        match c1, c2 with
        | Frame(space1, f1), Frame(space2, f2) -> Frame(RefSpace.Origin, Combine.PlacedShapes [(space1, f1); (space2, f2)])
        | Clips(space, clips), c
        | c , Clips(space, clips) when space = RefSpace.Origin -> Clips(space, c :: clips)
        | _ -> Clips(RefSpace.Origin, [c1; c2])

    type ShapesBuilder() =
        member x.Zero() = []
        member x.Yield(shape:RefSpace * Shape) = [shape]
        member x.YieldFrom(space:RefSpace, shapes:Shapes) = shapes |> List.map (fun (refSpace, shape) -> space + refSpace, shape)
        member x.Delay(f) = f()
        member x.Combine(f1, f2) = f1 @ f2
        member x.For(s, f:'T -> (RefSpace * Shape) list) = s |> Seq.map f |> Seq.fold (@) []

    type ClipsBuilder() =
        member x.Zero() = Frame(RefSpace.Origin, [])
        member x.Yield(space:RefSpace, shapes:Shapes) = Frame(space, shapes)
        member x.YieldFrom(space:RefSpace, c:Clip) = Clips(space, [c])
        member x.Delay(f) = f()
        member x.Combine(c1, c2) = combineClips c1 c2
        member x.For(s, f:'T -> Clip) = s |> Seq.map f |> Seq.reduce combineClips

    let shapes = new ShapesBuilder()
    let clips = new ClipsBuilder()

    let transformWith positions c = Transformation(LazyList.eval positions, c)

    open Animations
    open LazyList

    let parseScript (fullScript:ScriptedAnimation list) =
        let totalFrameCount = fullScript |> Seq.map (fun s -> s.StartFrame + s.FramesCount) |> Seq.max 
        let frames =
            fullScript
            |> Seq.collect (fun s -> seq { for i in 0 .. s.FramesCount - 1 do
                                           yield s.StartFrame + 1, { Animation = s.Animation; Target = s.Target; CurrentFrame = i; FramesCount = s.FramesCount } } )
            |> Seq.groupBy fst
            |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Seq.toList)
            |> Map.ofSeq

        lazylist {
            for i in 0 .. totalFrameCount - 1 do
            match frames.TryFind i with
            | Some frame -> yield frame
            | None -> yield [{Animation = Animation.None; Target = AnimationTarget.Global; CurrentFrame = 0; FramesCount = 1}]
        }

    let animateWith script animatable =
        Animation(RefSpace.Origin, animatable, script)
