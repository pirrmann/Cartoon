module SampleClips

open LazyList
open Shapes
open Cartoon

let scene1 = shapes { yield! [Point.Origin, RectangleFill(Vector(10, 10), Brush.Blue)
                              {x = 8; y = 60; z = 0}, EllipseFill(Vector(16, 8), Brush.Red)
                              {x = 8; y = 60; z = 0}, EllipseFill(Vector(8, 16), Brush.Red)] }

let clip1 = clips { yield {x=100; y=100; z=0}, scene1
                    yield {x=125; y=100; z=0}, scene1
                    yield {x=150; y=100; z=0}, scene1
                    yield {x=175; y=100; z=0}, scene1 }

let movie1 = clip1 |> transformWith (lazylist { for i in 1..10 do
                                                yield {x=100; y=i; z=0} })

let rec repeat s = seq { yield! s 
                         yield! repeat s }

let movie2 =
    clip1
    |> transformWith
        (lazylist {
                 yield! lazylist { for i in 1..25 do yield {x=0; y=i; z=0} }
                 yield! lazylist { for i in 1..25 do yield {x=i; y=25; z=0} }
                 yield! lazylist { for i in 1..25 do yield {x=25; y=25-i; z=0} }
                 yield! lazylist { for i in 1..25 do yield {x=25-i; y=0; z=0} }
             }
         |> LazyList.repeat) 

let clip3 = clips { yield! clip1
                    yield! movie2 }

let movie3 =
    clip3
    |> transformWith
        (lazylist {
            for i in 1..100 do
            yield {x=0; y=i; z=0}
         })

let head =
    clips {
        yield Point.Origin,
              shapes {
                yield {x = 0; y = 0; z=0}, Ellipse(Vector(300, 300), Pen.Red)
                yield {x = 50; y = -50; z=0}, EllipseFill(Vector(20, 20), Brush.Blue)
                yield {x = -50; y = -50; z=0}, EllipseFill(Vector(20, 20), Brush.Blue) }
        yield! Clip(
                    {x = 0; y = 50; z=0},
                    lazylist { for i in 10 .. 40 do
                               yield Frame({x = -50; y = 0; z=0}, [{x = 0; y = 0; z=0}, Bezier(Vector(100, 0), Vector(i, i), Vector(-i, i), { Pen.Green with Thickness = 5 })]) } |> eval |> holdOnLast)
    }

let test4 =
    Clips({x = 320; y = 240; z=0}, [head])
    |> transformWith
        (lazylist {
            for i in 1..100 do
            yield {x=0; y=i; z=0}
         })
