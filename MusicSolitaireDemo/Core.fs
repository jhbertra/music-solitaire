module Core

//
// --------- Validation Result ---------
//

type Error =
    | Stub

type Result<'a> =
    | Success of 'a
    | Failure of Error

let onSuccess result next = 
    match result with
    | Success a -> next a
    | Failure e -> Failure e

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