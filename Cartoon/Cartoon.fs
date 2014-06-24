module Cartoon

open Shapes
open LazyList

type Clip =
    | Frame of RefSpace * Shapes
    | Clip of RefSpace * LazyList<Clip>
    | Clips of RefSpace * Clip list
    | Transformation of LazyList<RefSpace> * Clip
    with member x.GetFrame() = match x with
                               | Frame(space, frame) -> Some(space, frame)
                               | Clip(space, frames) -> frames.Head |> Option.bind (fun (h, _) -> h.GetFrame()) |> Option.map(fun (relativeSpace, shapes) -> space + relativeSpace, shapes)
                               | Clips(space, clips) ->
                                    match clips |> List.choose (fun c -> c.GetFrame()) with
                                    | [] -> None
                                    | frames -> let relativeSpace, shapes = frames |> List.reduce combinePlacedElements
                                                Some(space + relativeSpace, shapes)
                               | Transformation(spaces, clip) -> spaces.Head |> Option.bind (fun (space, _) -> clip.GetFrame() |> Option.map(fun (relativeSpace, f) -> space + relativeSpace, f))

         member x.GetNext() = match x with
                              | Frame(_) -> Some(x)
                              | Clip(space, frames) -> frames.Head |> Option.map (fun (_, tail) -> Clip(space, eval tail))
                              | Clips(space, clips) ->
                                    match clips |> List.choose (fun c -> c.GetNext()) with
                                    | [] -> None
                                    | nextClips -> Some(Clips(space, nextClips))
                              | Transformation(spaces, clip) -> spaces.Head
                                                                |> Option.map (fun(_, tail) -> eval tail)
                                                                |> Option.bind (fun tail ->
                                                                    clip.GetNext()
                                                                    |> Option.map(fun next ->
                                                                       Transformation(tail, next)))
         member x.RelativeTo(space: RefSpace) = match x with
                                                | Frame(otherSpace, f) -> Frame(otherSpace - space, f)
                                                | Clip(otherSpace, frames) -> Clip(otherSpace - space, frames)
                                                | Clips(otherSpace, clips) -> Clips(otherSpace - space, clips)
                                                | Transformation(ps, clip) -> Transformation(ps |> LazyList.map (fun (otherSpace) -> otherSpace - space), clip)

let rec combineClips c1 c2 =
    match c1, c2 with
    | Frame(space1, f1), Frame(space2, f2) -> combinePlacedElements (space1, f1) (space2, f2) |> Frame
    | Clips(space, clips), c
    | c , Clips(space, clips) ->  Clips(space, c.RelativeTo(space) :: clips)
    | _ -> Clips(RefSpace.Origin, [c1; c2])

type ShapesBuilder() =
    member x.Zero() = []
    member x.Yield(shape:RefSpace * Shape) = [shape]
    member x.YieldFrom(shapes:Shapes) = shapes
    member x.YieldFrom(c:Clip) = match c.GetFrame() with
                                 | Some(space, f) -> f |> List.map (fun (relativeSpace, s) -> space + relativeSpace, s)
                                 | None -> []
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

let transformWith positions c = Transformation(eval positions, c)
