module Game

open Core

//
// --------- Model ---------
//

type Suit =
    | Hearts
    | Clubs
    | Diamonds
    | Spades
let suits = [ Hearts; Clubs; Diamonds; Spades ]

type Face =
    | KeySignature
    | Do
    | Re
    | Me
    | Fa
    | So
    | La
    | Ti
    | Do8
    | IV
    | V
    | I
let faces = [ KeySignature; Do; Re; Me; Fa; So; La; Ti; Do8; IV; V; I ]

type Card = Card of Suit * Face

type TableauNumber =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven

type Piles = {
    stock : Card list
    talon : Card list
    tableau1 : Card list
    tableau2 : Card list * Card list
    tableau3 : Card list * Card list
    tableau4 : Card list * Card list
    tableau5 : Card list * Card list
    tableau6 : Card list * Card list
    tableau7 : Card list * Card list
    heartsFoundation : Card list
    spadesFoundation : Card list
    diamondsFoundation : Card list
    clubsFoundation : Card list
    }

type DealPhaseModel = {
    deck : Card list
    tableauDealMoves : (TableauNumber*bool) list
    }

type GamePhase =
    | DealPhase of DealPhaseModel
    | PlayingPhase
    | DonePhase

type Model = {
    phase : GamePhase
    piles : Piles
    }



//
// --------- Init ---------
//

let initCards = 
    [for suit in suits do
        for face in faces do
            yield Card (suit, face)]

let initTableauDealMoves = 
    let numberOrder = [One;Two;Three;Four;Five;Six;Seven]
    numberOrder
    |> List.mapi (fun i _ -> (List.skip i numberOrder) |> List.mapi (fun j num -> num,j=0))
    |> List.collect (fun pass -> pass)

let initPiles = {
    stock = []
    talon = []
    tableau1 = []
    tableau2 = [],[]
    tableau3 = [],[]
    tableau4 = [],[]
    tableau5 = [],[]
    tableau6 = [],[]
    tableau7 = [],[]
    heartsFoundation = []
    spadesFoundation = []
    diamondsFoundation = []
    clubsFoundation = []
    }

let initModel rng = {
    phase = DealPhase {
        deck = 
            initCards
            |> Array.ofList
            |> shuffleArr rng
            |> List.ofArray
        tableauDealMoves = initTableauDealMoves
        }
    piles = initPiles
    }



//
// --------- Msg ---------
//

type Msg =
    | DealNextCard



//
// --------- Update ---------
//

let updateTableau tableau faceUp card =
    match tableau with up,down -> if faceUp then card::up,down else up,card::down

let update msg model =
    match model.phase with

    | DealPhase dealModel -> 
        match msg with
        | DealNextCard ->
            match dealModel.deck with
            | [] -> { model with phase = PlayingPhase },Term
            | card :: remaining -> 
                let newPiles,newMoves =
                    match dealModel.tableauDealMoves with
                    | move :: tail -> 
                        match move with
                        | One,_ -> { model.piles with tableau1 = [card] },tail
                        | Two,faceUp -> { model.piles with tableau2 = updateTableau model.piles.tableau2 faceUp card },tail
                        | Three,faceUp -> { model.piles with tableau3 = updateTableau model.piles.tableau3 faceUp card },tail
                        | Four,faceUp -> { model.piles with tableau4 = updateTableau model.piles.tableau4 faceUp card },tail
                        | Five,faceUp -> { model.piles with tableau5 = updateTableau model.piles.tableau5 faceUp card },tail
                        | Six,faceUp -> { model.piles with tableau6 = updateTableau model.piles.tableau6 faceUp card },tail
                        | Seven,faceUp -> { model.piles with tableau7 = updateTableau model.piles.tableau7 faceUp card },tail
                    | [] -> { model.piles with stock = card::model.piles.stock },[]
                { model with 
                    phase = DealPhase { deck = remaining; tableauDealMoves = newMoves }
                    piles = newPiles
                    }
                ,Msg DealNextCard

    | PlayingPhase -> model,Term
    | DonePhase -> model,Term