module Cartoon

open Shapes
open LazyList

type Clip =
    | Frame of Point * Shapes
    | Clip of Point * LazyList<Clip>
    | Clips of Point * Clip list
    | Transformation of LazyList<Point> * Clip
    with member x.GetFrame() = match x with
                               | Frame(point, frame) -> Some(point, frame)
                               | Clip(point, frames) -> frames.Head |> Option.bind (fun (h, _) -> h.GetFrame()) |> Option.map(fun (p2, shapes) -> point + p2, shapes)
                               | Clips(point, clips) ->
                                    match clips |> List.choose (fun c -> c.GetFrame()) with
                                    | [] -> None
                                    | frames -> let p2, shapes = frames |> List.reduce combinePlacedElements
                                                Some(point + p2, shapes)
                               | Transformation(positions, clip) -> positions.Head |> Option.bind (fun (p, _) -> clip.GetFrame() |> Option.map(fun (p2, f) -> p + p2, f))

         member x.GetNext() = match x with
                              | Frame(_) -> Some(x)
                              | Clip(point, frames) -> frames.Head |> Option.map (fun (_, tail) -> Clip(point,  eval tail))
                              | Clips(point, clips) ->
                                    match clips |> List.choose (fun c -> c.GetNext()) with
                                    | [] -> None
                                    | nextClips -> Some(Clips(point, nextClips))
                              | Transformation(positions, clip) -> positions.Head
                                                                   |> Option.map (fun(_, tail) -> eval tail)
                                                                   |> Option.bind (fun tail ->
                                                                       clip.GetNext()
                                                                       |> Option.map(fun next ->
                                                                          Transformation(tail, next)))
         member x.RelativeTo(p: Point) = match x with
                                         | Frame(p2, f) -> Frame(p2 - p, f)
                                         | Clip(p2, frames) -> Clip(p2 - p, frames)
                                         | Clips(p2, clips) -> Clips(p2 - p, clips)
                                         | Transformation(ps, clip) -> Transformation(ps |> LazyList.map (fun (p2) -> p2-p), clip)

let rec combineClips c1 c2 =
    match c1, c2 with
    | Frame(p1, f1), Frame(p2, f2) -> combinePlacedElements (p1, f1) (p2, f2) |> Frame
    | Clips(p, clips), c
    | c , Clips(p, clips) ->  Clips(p, c.RelativeTo(p) :: clips)
    | _ -> Clips(Point.Origin, [c1; c2])

type ShapesBuilder() =
    member x.Zero() = []
    member x.Yield(shape:Point * Shape) = [shape]
    member x.YieldFrom(shapes:Shapes) = shapes
    member x.YieldFrom(c:Clip) = match c.GetFrame() with
                                 | Some(p, f) -> f |> List.map (fun (p', s) -> p + p', s)
                                 | None -> []
    member x.Delay(f) = f()
    member x.Combine(f1, f2) = f1 @ f2

type ClipsBuilder() =
    member x.Zero() = Frame(Point.Origin, [])
    member x.Yield(p:Point, shapes:Shapes) = Frame(p, shapes)
    member x.YieldFrom(c:Clip) = c
    member x.Delay(f) = f()
    member x.Combine(c1, c2) = combineClips c1 c2

let shapes = new ShapesBuilder()
let clips = new ClipsBuilder()
