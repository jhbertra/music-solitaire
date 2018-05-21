module Tests

open FsGame.Core
open FSharpPlus
open Model
open System
open UseCases
open Xunit

[<Fact>]
let ``Check for win sets won to true under winning conditions`` () =
    let model = initialize (new Random())
    let model =
        { model with
            heartsFoundation = Foundation ([for face in faces do yield Card (Hearts, face)], Hearts)
            spadesFoundation = Foundation ([for face in faces do yield Card (Spades, face)], Spades)
            diamondsFoundation = Foundation ([for face in faces do yield Card (Diamonds, face)], Diamonds)
            clubsFoundation = Foundation ([for face in faces do yield Card (Clubs, face)], Clubs) }
    let (model, _) = checkForWin (konst (fun {model=model} -> UpdateResult (model, []))) model
    Assert.True(model.won)
