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
        member x.YieldFrom(shapes:Shapes) = shapes
        member x.Delay(f) = f()
        member x.Combine(f1, f2) = f1 @ f2

    type ClipsBuilder() =
        member x.Zero() = Frame(RefSpace.Origin, [])
        member x.Yield(space:RefSpace, shapes:Shapes) = Frame(space, shapes)
        member x.YieldFrom(c:Clip) = c
        member x.Delay(f) = f()
        member x.Combine(c1, c2) = combineClips c1 c2

    let shapes = new ShapesBuilder()
    let clips = new ClipsBuilder()

    let transformWith positions c = Transformation(LazyList.eval positions, c)
