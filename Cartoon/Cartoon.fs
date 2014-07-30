namespace FCartoon

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
type Clip =
    | Frame of PlacedShapes
    | Clip of RefSpace * LazyList<Clip>
    | Clips of RefSpace * Clip list
    | Transformation of LazyList<RefSpace> * Clip with
    member x.GetFrame() = match x with
                          | Frame(space, frame) -> Some(space, frame)
                          | Clip(space, frames) -> frames.Head |> Option.bind (fun (h, _) -> h.GetFrame()) |> Option.map(fun (relativeSpace, shapes) -> space + relativeSpace, shapes)
                          | Clips(space, clips) ->
                              match clips |> List.choose (fun c -> c.GetFrame()) with
                              | [] -> None
                              | frames -> let shapes = Combine.PlacedShapes frames
                                          Some(space, shapes)
                          | Transformation(spaces, clip) -> spaces.Head |> Option.bind (fun (space, _) -> clip.GetFrame() |> Option.map(fun (relativeSpace, f) -> space + relativeSpace, f))

    member x.GetNext() = match x with
                         | Frame(_) -> Some(x)
                         | Clip(space, frames) -> frames.Head |> Option.map (fun (_, tail) -> Clip(space, LazyList.eval tail))
                         | Clips(space, clips) ->
                             match clips |> List.choose (fun c -> c.GetNext()) with
                             | [] -> None
                             | nextClips -> Some(Clips(space, nextClips))
                         | Transformation(spaces, clip) ->
                            spaces.Head
                            |> Option.map (fun(_, tail) -> LazyList.eval tail)
                            |> Option.bind (fun tail ->
                                clip.GetNext()
                                |> Option.map(fun next ->
                                    Transformation(tail, next)))
    member x.RefSpace() = match x with
                          | Frame(space, _)
                          | Clip(space, _)
                          | Clips(space, _) -> space
                          | Transformation(spaces, clip) -> defaultArg (spaces.Head |> Option.map fst) (RefSpace.Origin)
