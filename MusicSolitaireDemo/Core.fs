module Core

open System

//
// --------- State ---------
//

type State<'s,'v> = State of ('s -> 'v * 's)

let runState state initial =
    match state with State f -> f initial

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

let evalState state initial =
    runState state initial
    |> fst

let execState state initial =
    runState state initial
    |> snd



//
// --------- Other Stuff ---------
//

type Cmd<'a> =
    | Term
    | Msg of 'a

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