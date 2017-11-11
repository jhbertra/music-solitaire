module Core

//
// --------- State ---------
//

type State<'s,'v> = State of ('s -> 'v * 's)

let runState state initial =
    match state with State f -> f initial

type StateBuilder() =

    member this.Return(x) =
        State (fun s -> x,s)

    member this.Bind(m, f) =
        State (fun s ->
            let (v, newState) = runState m s
            runState (f v) newState)

    member this.Zero() =
        State(fun s -> (),s)



let get = State (fun s -> (s,s))

let put newState = State (fun s -> ((), newState))

let modify f =
    let state = new StateBuilder() 
    state {
        let! x = get
        do! put (f x)
    }

let gets f =
    let state = new StateBuilder() 
    state {
        let! x = get
        return f x
    }

let evalState state initial =
    runState state initial
    |> fst

let execState state initial =
    runState state initial
    |> snd



//
// --------- Result ---------
//

type Result<'a,'e> =
    | Success of 'a
    | Failure of 'e

type ResultBuilder() =

    member this.Bind(m, f) = 
        match m with
        | Success a -> f a
        | Failure e -> Failure e

    member this.Return(x) = x



//
// --------- Other Stuff ---------
//

let shuffle (rng: System.Random) arr =
    let array = Array.copy arr
    let n = array.Length
    for x in 1..n do
        let i = n-x
        let j = rng.Next(i+1)
        let tmp = array.[i]
        array.[i] <- array.[j]
        array.[j] <- tmp
    array