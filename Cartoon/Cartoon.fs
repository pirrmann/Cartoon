module Cartoon

open System
open System.Drawing

open Shapes
open LazyList

type Id = | Id of int
let currentId = ref (Id 0)
let getNewId () =
    match !currentId with
    | Id(counter) ->
        currentId := Id(counter + 1)
        !currentId
    

type Clip =
    | Frame of Id * Point * Shapes
    | Clip of Id * Point * Clip list
    | Movie of Id * LazyList<Point * Clip>
    with member x.GetFrame() = match x with
                               | Frame(id, point, frame) -> Some(point, frame)
                               | Clip(id, point, clips) ->
                                    match clips |> List.choose (fun c -> c.GetFrame()) with
                                    | [] -> None
                                    | frames -> Some(frames |> List.reduce combinePlacedElements)
                               | Movie(id, frames) -> frames.Head |> Option.bind (fun ((p, c), _) -> c.GetFrame() |> Option.map(fun (p2, f) -> p + p2, f))

         member x.GetId() = match x with
                            | Frame(id, _, _)
                            | Clip(id, _, _)
                            | Movie(id, _) -> id
                                                 
         member x.GetNext() =
                              //printfn "GetNext() on id %A" (x.GetId())
                              match x with
                              | Frame(_) -> Some(x)
                              | Clip(id, point, clips) ->
                                    match clips |> List.choose (fun c -> c.GetNext()) with
                                    | [] -> None
                                    | nextClips -> Some(Clip(getNewId(), point, nextClips))
                              | Movie(id, frames) -> frames.Head
                                                     |> Option.map (fun(_, tail) -> eval tail)
                                                     |> Option.map (fun tail ->
                                                        let nextFrames = tail |> LazyList.choose (fun (p, c) -> c.GetNext() |> Option.map (fun c' -> p, c'))
                                                        Movie(getNewId(), nextFrames))
         member x.RelativeTo(p: Point) = match x with
                                         | Frame(id, p2, f) -> Frame(getNewId(), p2 - p, f)
                                         | Clip(id, p2, clips) -> Clip(getNewId(), p2 - p, clips)
                                         | Movie(id, s) -> Movie(getNewId(), s |> LazyList.map (fun (p2, c) -> p2-p, c))

let rec combineClips c1 c2 =
    match c1, c2 with
    | Frame(_, p1, f1), Frame(_, p2, f2) -> combinePlacedElements (p1, f1) (p2, f2) |> (fun (p, ss) -> Frame(getNewId(), p, ss))
    | Movie(_, _), Frame(_, _, _)
    | Frame(_, _, _), Movie(_, _)
    | Movie(_, _), Movie(_, _) -> Clip(getNewId(), Point.Origin, [c1; c2])
    | Clip(_, p, clips), c
    | c , Clip(_, p, clips) ->  Clip(getNewId(), p, c.RelativeTo(p) :: clips)

type ShapesBuilder() =
    member x.Zero() = []
    member x.Yield(shape:Point * Shape) = [shape]
    member x.YieldFrom(shapes:Shapes) = shapes
    member x.YieldFrom(c:Clip) = match c.GetFrame() with
                                 | Some(p, f) -> f |> List.map (fun (p', s) -> p + p', s)
                                 | None -> []
    member x.Delay(f) = f()
    member x.Combine(f1, f2) = f1 @ f2

type ClipBuilder() =
    member x.Zero() = Frame(getNewId(), Point.Origin, [])
    member x.Yield(p:Point, shapes:Shapes) = Frame(getNewId(), p, shapes)
    member x.YieldFrom(c:Clip) = c
    member x.Delay(f) = f()
    member x.Combine(c1, c2) = combineClips c1 c2

let shapes = new ShapesBuilder()
let clip = new ClipBuilder()

let play (graphics:Graphics) (clip:Clip) =
    match clip.GetFrame() with
    | Some(point, frame) ->
        use image = new Bitmap(640, 480, graphics)
        use g2 = Graphics.FromImage(image)
        draw g2 (point, frame)
        graphics.DrawImage(image, 0, 0)
    | None -> ()