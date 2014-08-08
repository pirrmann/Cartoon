namespace FCartoon

open Animations

[<ReflectedDefinition>]
module internal Combine =
    let PlacedShapes (placedShapes:(RefSpace * Shapes) list) = 
        seq {
            for space, shapes in placedShapes do
            for relativeSpace, shape in shapes do
            yield space + relativeSpace, shape
        } |> Seq.toList

[<ReflectedDefinition>]
type PlacedShapes = RefSpace * Shapes

[<ReflectedDefinition>]
type TransformationScript = LazyList<RefSpace>

[<ReflectedDefinition>]
type AnimationScript = LazyList<AnimationFrames>

[<ReflectedDefinition>]
type IAnimatable = abstract member GetFrame: AnimationFrames -> Shapes

[<ReflectedDefinition>]
type Clip =
    | Frame of PlacedShapes
    | Clip of RefSpace * LazyList<Clip>
    | Clips of RefSpace * Clip list
    | Transformation of TransformationScript * Clip
    | Animation of RefSpace * IAnimatable * AnimationScript with
    member x.GetFrame() = match x with
                          | Frame(space, frame) -> Some(space, frame)
                          | Clip(space, frames) -> frames.Head |> Option.bind (fun (h, _) -> h.GetFrame()) |> Option.map(fun (relativeSpace, shapes) -> space + relativeSpace, shapes)
                          | Clips(space, clips) ->
                              match clips |> List.choose (fun c -> c.GetFrame()) with
                              | [] -> None
                              | frames -> let shapes = Combine.PlacedShapes frames
                                          Some(space, shapes)
                          | Transformation(script, clip) -> script.Head |> Option.bind (fun (space, _) -> clip.GetFrame() |> Option.map(fun (relativeSpace, f) -> space + relativeSpace, f))
                          | Animation(space, animatable, script) -> script.Head |> Option.map (fun (animation, _) -> space, animatable.GetFrame(animation))

    member x.GetNext() = match x with
                         | Frame(_) -> Some(x)
                         | Clip(space, frames) -> frames.Head |> Option.map (fun (_, tail) -> Clip(space, LazyList.eval tail))
                         | Clips(space, clips) ->
                             match clips |> List.choose (fun c -> c.GetNext()) with
                             | [] -> None
                             | nextClips -> Some(Clips(space, nextClips))
                         | Transformation(script, clip) ->
                            script.Head
                            |> Option.map (fun(_, tail) -> LazyList.eval tail)
                            |> Option.bind (fun tail ->
                                clip.GetNext()
                                |> Option.map(fun next ->
                                    Transformation(tail, next)))
                         | Animation(space, animatable, script) ->
                            script.Head
                            |> Option.map (fun(_, tail) -> Animation(space, animatable, LazyList.eval tail))
    member x.RefSpace() = match x with
                          | Frame(space, _)
                          | Clip(space, _)
                          | Clips(space, _)
                          | Animation(space, _, _) -> space
                          | Transformation(script, _) -> defaultArg (script.Head |> Option.map fst) (RefSpace.Origin)
