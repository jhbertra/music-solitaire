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

//
// --------- State ---------
//

type State<'s,'v> = State of ('s -> 'v * 's)

let runState initial state = match state with State f -> f initial

type StateBuilder() =

    member inline this.Return(x) = State (fun s -> x,s)

    member inline this.ReturnFrom(m) = m

    member this.Delay f = f ()

    member inline this.Bind(m, f) = 
        State (fun s ->
            let (v, newState) = runState s m
            runState newState (f v))

    member this.Combine (x1, x2) =
            State(fun state ->
                let result, state = runState state x1
                runState state x2)

    member this.For (seq, f) =
            seq
            |> Seq.map f
            |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))

    member inline this.Zero() = State(fun s -> (),s)

let state = StateBuilder()

let getState = State (fun s -> (s,s))

let putState newState = State (fun s -> ((), newState))

let modify f =
    state {
        let! x = getState
        do! putState (f x)
    }

let gets f =
    state {
        let! x = getState
        return f x
    }

let evalState initial state = runState initial state |> fst

let execState initial state = runState initial state |> snd



//
// --------- Platform ---------
//

type ContentManifest = {
    root : string
    textures : string list
    sfx: string list
}

type SoundMode =
    | Overlap
    | NoOverlap

type GameTime = {
    elapsed : TimeSpan
    total : TimeSpan
    isRunningSlowly : bool
}

type Point = Point of float*float

type Touch = Touch of int * Point

let id (Touch (id,_)) = id

let position (Touch (_,pos)) = pos

type BoundingBox = BoundingBox of float*float*float*float

let union (BoundingBox (xa,ya,wa,ha)) (BoundingBox (xb,yb,wb,hb)) = 
    let x1a = xa + wa
    let x1b = xb + wb
    let y1a = ya + ha
    let y1b = yb + hb
    let xc = min xa xb
    let yc = min ya yb
    let x1c = max x1a x1b
    let y1c = max y1a y1b
    let wc = x1c - xc
    let hc = y1c - yc
    BoundingBox (xc, yc, wc, hc)

let intersect (BoundingBox (xa,ya,wa,ha)) (BoundingBox (xb,yb,wb,hb)) =
    let x1a = xa + wa
    let x1b = xb + wb
    let y1a = ya + ha
    let y1b = yb + hb
    let xc = max xa xb
    let yc = max ya yb
    let x1c = min x1a x1b
    let y1c = min y1a y1b
    let wc = x1c - xc
    let hc = y1c - yc
    if wc < 0.0 || hc < 0.0 then
        None
    else
        Some (BoundingBox (xc, yc, wc, hc))

type GameObject<'a> = {
    textures : string list
    box : BoundingBox
    alpha : float
    tag : 'a
}

let bounds { box = BoundingBox (x,y,w,h) } = x,y,w,h

let tag { tag = tag } = tag

let box { box = box} = box

type GameObjectDrawRequest<'a> =
    | Area of BoundingBox * 'a
    | Sprite of string list * Point * float * 'a

type GameState<'m, 't> = {
    touches : Touch list
    gameTime : GameTime
    objects : GameObject<'t> list
    model : 'm
}

let gameState touches gameTime objects model = { touches = touches; gameTime = gameTime; objects = objects; model = model; }

type Cmd<'m, 't> =
    | PlaySound of string * SoundMode * float
    | Delay of float * Update<'m, 't>
and UpdateResult<'m, 't> = UpdateResult of 'm * Cmd<'m, 't> list
and Update<'m, 't> = GameState<'m, 't> -> UpdateResult<'m, 't>

type Draw<'m, 't> = 'm -> GameObjectDrawRequest<'t> list

type GameEngine<'m, 't> = {
    contentManifest : ContentManifest
    init : 'm
    update : Update<'m, 't>
    draw : Draw<'m, 't>
}

//
// --------- Other Stuff ---------
//

let shuffleArr (rng: System.Random) arr =
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