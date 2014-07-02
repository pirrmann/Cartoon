namespace FCartoon

[<ReflectedDefinition>]
module Player =
    let rec play draw (clip:Clip) =
        async { 
            match clip.GetFrame() with
            | Some(space, frame) ->
                draw (space, frame)
                do! Async.Sleep(42)
                match clip.GetNext() with
                | Some nextClip ->
                    do! play draw nextClip
                | None -> ()            
            | None -> () }
