module Core

//
// --------- State ---------
//

type State<'s,'v> = State of ('s -> 'v * 's)

let runState state initial =
    match state with State f -> f initial

let returnState v = State (fun s -> v,s)

let bindState m f = 
    State (fun s ->
        let (v, newState) = runState m s
        runState (f v) newState)

type StateBuilder() =
    member this.Return(x) = returnState x
    member this.ReturnFrom(m) = m
    member this.Bind(m, f) = bindState m f
    member this.Zero() = State(fun s -> (),s)

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
// --------- Result ---------
//

type Result<'a,'e> =
    | Success of 'a
    | Failure of 'e

let returnResult x = Success x

let bindResult m f = 
    match m with
    | Success a -> f a
    | Failure e -> Failure e

type ResultBuilder() =
    member this.Bind(m, f) = bindResult
    member this.Return(x) = returnResult

let result = new ResultBuilder()



//
// --------- Cmd ---------
//

type Cmd<'a> =
    | Term
    | Msg of 'a

let rec runCmd updateFn cmd = 
    state {
        let! model = getState
        return! 
            match cmd with
            | Term -> returnState model
            | Msg msg -> 
                state {
                    let newModel,newCmd = updateFn msg model
                    do! putState newModel
                    return! runCmd updateFn newCmd
                }
        }

let runGameState updateFn cmd model =
    let gameStateBuilder = runCmd updateFn cmd
    runState gameStateBuilder model


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