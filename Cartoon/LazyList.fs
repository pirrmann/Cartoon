﻿namespace FCartoon

[<ReflectedDefinition>]
type LazyList<'T> =
    | Empty
    | Singleton of 'T
    | LazyCons of 'T * Lazy<LazyList<'T>>
    | LazyConcat of LazyList<'T> * Lazy<LazyList<'T>>
    with member x.Head =  match x with
                            | Empty -> None
                            | Singleton(x) -> Some(x, lazy Empty)
                            | LazyCons(h, t) -> Some(h, t)
                            | LazyConcat(first, next) ->
                                match first.Head with
                                | None -> next.Value.Head
                                | Some(head, tail) ->
                                    match tail.Value with
                                    | Empty -> Some(head, next)
                                    | _ -> Some(head, lazy LazyConcat(tail.Value, next))

[<ReflectedDefinition>]
module LazyList =
    let eval (x:Lazy<'T>) = x.Value

    let rec map f (ll:Lazy<LazyList<'a>>) = lazy (
        match (eval ll).Head with
        | None -> Empty
        | Some(head, tail) ->
            LazyCons(f head, map f tail))

    let rec choose f (ll:LazyList<'a>) =
        match ll.Head with
        | None -> Empty
        | Some(head, tail) ->
            match f head with
            | Some v -> LazyCons(v, lazy choose f (tail.Value))
            | None -> choose f (tail.Value)

    let rec toSeq (ll:LazyList<'a>) =
        seq {
            match ll.Head with
            | None -> ()
            | Some(head, tail) ->
                yield head
                yield! tail.Value |> toSeq }

    let rec repeat loop = lazy LazyConcat(eval loop, repeat loop)

    let rec holdOnLast (ll:LazyList<'a>) =
        match ll.Head with
        | None -> Empty
        | Some(head, tail) ->
            match eval tail with
            | Empty -> LazyConcat(ll, lazy holdOnLast ll)
            | _ -> LazyCons(head, lazy holdOnLast (eval tail))

    let rec last (ll:LazyList<'a>) =
        match ll.Head with
        | None -> None
        | Some(head, tail) ->
            match eval tail with
            | Empty -> Some head
            | _ -> last (eval tail)  

    type LazyListBuilder() =
        let concat (x:LazyList<'T>) (y:Lazy<LazyList<'T>>) =
            match x with
            | Empty -> y.Value
            | Singleton v -> LazyCons(v, y)
            | _ -> LazyConcat(x, y)

        let rec mapSeq (s:seq<'T>) (f:'T->LazyList<'U>) =
            if Seq.isEmpty s then Empty
            else concat (f (Seq.head s)) (lazy mapSeq (s |> Seq.skip 1) f)

        member this.Zero() = Empty
        member this.Yield(x) = Singleton(x)
        member this.YieldFrom(x) = eval x
        member this.For(s, f) = mapSeq s f
        member this.Delay(f) = lazy f()
        member this.Combine(x, y) = concat x y

    let lazylist = new LazyListBuilder()
