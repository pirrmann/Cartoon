module SampleClips

open LazyList
open Shapes
open Cartoon

let scene1 = shapes { yield! [RefSpace.Origin, RectangleFill(Vector(10.0, 10.0), Brush.Blue)
                              {x = 8.0; y = 60.0; z = 0.0}, EllipseFill(Vector(16.0, 8.0), Brush.Red)
                              {x = 8.0; y = 60.0; z = 0.0}, EllipseFill(Vector(8.0, 16.0), Brush.Red)] }

let clip1 = clips { yield {x=100.0; y=100.0; z=0.0}, scene1
                    yield {x=125.0; y=100.0; z=0.0}, scene1
                    yield {x=150.0; y=100.0; z=0.0}, scene1
                    yield {x=175.0; y=100.0; z=0.0}, scene1 }

let movie1 = clip1 |> transformWith (lazylist { for i in 1..10 do
                                                yield {x = 100.0; y = float i; z = 0.0} })

let rec repeat s = seq { yield! s 
                         yield! repeat s }

let movie2 =
    clip1
    |> transformWith
        (lazylist {
                 yield! lazylist { for i in 1..25 do yield {x = 0.0; y = float i; z = 0.0} }
                 yield! lazylist { for i in 1..25 do yield {x = float i; y = 25.0; z = 0.0} }
                 yield! lazylist { for i in 1..25 do yield {x = 25.0; y = 25.0 - float i; z = 0.0} }
                 yield! lazylist { for i in 1..25 do yield {x = 25.0 - float i; y = 0.0; z = 0.0} }
             }
         |> LazyList.repeat) 

let clip3 = clips { yield! clip1
                    yield! movie2 }

let movie3 =
    clip3
    |> transformWith
        (lazylist {
            for i in 1..100 do
            yield {x = 0.0; y = float i; z = 0.0}
         })

let head =
    clips {
        yield RefSpace.Origin,
              shapes {
                yield {x = 0.0; y = 0.0; z = 0.0}, Ellipse(Vector(300.0, 300.0), Pen.Red)
                yield {x = 50.0; y = -50.0; z = 0.0}, EllipseFill(Vector(20.0, 20.0), Brush.Blue)
                yield {x = -50.0; y = -50.0; z = 0.0}, EllipseFill(Vector(20.0, 20.0), Brush.Blue) }
        yield! Clip(
                    {x = 0.0; y = 50.0; z=0.0},
                    lazylist { for i in 10 .. 40 do
                               yield Frame({x = -50.0; y = 0.0; z = 0.0}, [{x = 0.0; y = 0.0; z = 0.0}, Bezier(Vector(100.0, 0.0), Vector(float i, float i), Vector(float (-i), float i), { Pen.Green with Thickness = 5 })]) } |> eval |> holdOnLast)
    }

let test4 =
    Clips({x = 320.0; y = 240.0; z = 0.0}, [head])
    |> transformWith
        (lazylist {
            for i in 1..50 do
            yield {x = 0.0; y = float i; z = 0.0}
         })
