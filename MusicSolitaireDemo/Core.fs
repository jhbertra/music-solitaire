module Core

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