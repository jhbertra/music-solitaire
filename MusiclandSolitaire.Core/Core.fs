module Core

open System

//
// --------- Optional ---------
//

type OptionalBuilder() =

    member __.Bind(opt, binder) =
        match opt with
        | Some value -> binder value
        | None -> None

    member __.Return(value) =
        Some value

    member __.ReturnFrom(o) =
        o

    member __.Zero() =
        None
    
let optional = OptionalBuilder()

let defaultIfNone defaultValue option =
    match option with
    | None -> defaultValue
    | Some value -> value

let optionToSingletonList = function
| None -> []
| Some x -> [x]

let mapOption f option =
    match option with
    | None -> None
    | Some value -> Some (f value)

let rec values = function
| [] -> []
| Some x :: xs -> x :: values xs
| None :: xs -> [] @ values xs

let (<?>) f arg = mapOption (fun f -> f arg) f

//
// --------- Other Stuff ---------
//

let shuffleArr (rng: Random) arr =
    let array = Array.copy arr
    let n = array.Length
    for x in 1..n do
        let i = n-x
        let j = rng.Next(i+1)
        let tmp = array.[i]
        array.[i] <- array.[j]
        array.[j] <- tmp
    array

let mapT2 f t2 =
    let a,b = t2
    (f a, f b)

let bimapT2 f1 f2 t2 =
    let a,b = t2
    (f1 a, f2 b)

let selectT2 f1 f2 x =
    (f1 x, f2 x)

let mapT4 f t4 =
    let a,b,c,d = t4
    (f a, f b, f c, f d)

let fullJoin list1 list2 key1Selector key2Selector =
    let notInList list keySelector = (fun x -> List.map keySelector list |> List.contains x |> not)
    let list1MissingKeys = List.filter (notInList list1 key1Selector) (list2 |> List.map key2Selector)
    let list2MissingKeys = List.filter (notInList list2 key2Selector) (list1 |> List.map key1Selector)
    let list1Full =
        (list1 |> List.map Some |> List.zip (list1 |> List.map key1Selector))
        @ (list1MissingKeys |> List.map (fun i -> i,None))
        |> List.sortBy fst
    let list2Full =
        (list2 |> List.map Some |> List.zip (list2 |> List.map key2Selector))
        @ (list2MissingKeys |> List.map (fun i -> i,None))
        |> List.sortBy fst
    List.zip list1Full list2Full |> List.map (bimapT2 snd snd)

let applyT2 f arg1 arg2 = f (arg1, arg2)

let flip f a1 a2 = f a2 a1

let idFunc x = x