namespace FCartoon

[<ReflectedDefinition>]
module Curves =

    type CurveType =
        | Segment
    type CurveSection = CurveType * float * float
    type Curve = float * CurveSection list

    let curve c x =
        let rec curve' (x1, y1) (sections:CurveSection list) x = 
            match sections with
            | (_, x2, y2) :: _ when x <= x2 -> y1 + (x - x1) * (y2 - y1) / (x2 - x1)
            | (_, x2, y2) :: tail -> curve' (x2, y2) tail x
            | [] -> 1.0

        let y0, sections = c

        let curved = curve' (0.0, y0) sections x
        if curved = 0.0 then 0.001 else curved

    let Up =
        0.0,
        [ Segment, 1.0, 1.0 ]

    let Down =
        1.0,
        [ Segment, 1.0, 0.0 ]

    let UpDown =
        0.0,
        [ Segment, 0.5, 1.0
          Segment, 1.0, 0.0 ]

    let UpFlatDown flatRatio =
        0.0,
        [ Segment, (1.0 - flatRatio) / 2.0, 1.0
          Segment, 1.0 - (1.0 - flatRatio) / 2.0, 1.0
          Segment, 1.0, 0.0 ]

    let DownUp =
        1.0,
        [ Segment, 0.5, 0.0
          Segment, 1.0, 1.0 ]
