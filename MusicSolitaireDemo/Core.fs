module Core

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

//
// --------- State ---------
//

type State<'s,'v> = State of ('s -> 'v * 's)

let runState state initial = match state with State f -> f initial

type StateBuilder() =

    member inline this.Return(x) = State (fun s -> x,s)

    member inline this.ReturnFrom(m) = m

    member inline this.Bind(m, f) = 
        State (fun s ->
            let (v, newState) = runState m s
            runState (f v) newState)

    member inline this.Zero() = State(fun s -> (),s)

let state = new StateBuilder()

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

let evalState state initial = runState state initial |> fst

let execState state initial = runState state initial |> snd



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

type Cmd<'a> =
    | Term
    | Msg of 'a
    | PlaySound of string * SoundMode * Cmd<'a>
    | Delay of float * Cmd<'a>

type Sprite<'a> = {
    textures : string list
    position : float*float
    touchDown : (float*float -> 'a) option
    touchMoved : (float*float -> 'a) option
    touchUp : (float*float -> 'a) option
    tapped : 'a option
    alpha : float
}

type Sub<'a> = TouchDropped of 'a

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