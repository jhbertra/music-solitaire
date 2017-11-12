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

let suits = [ Hearts; Clubs; Diamonds; Spades ]
let faces = 
    [ 
    KeySignature
    Do
    Re
    Me
    Fa
    So
    La
    Ti
    Do8
    IV
    V
    I
    ]

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

let tableauDealMoves = [
    One,true
    Two,false
    Three,false
    Four,false
    Five,false
    Six,false
    Seven,false

    Two,true
    Three,false
    Four,false
    Five,false
    Six,false
    Seven,false

    Three,true
    Four,false
    Five,false
    Six,false
    Seven,false

    Four,true
    Five,false
    Six,false
    Seven,false

    Five,true
    Six,false
    Seven,false

    Six,true
    Seven,false

    Seven,true
    ]

type InitModel = {
    deck : Card list
    tableauDealMoves : (TableauNumber*bool) list
    piles : Piles
    }

type Model = 
    | InitPhase of InitModel
    | PlayingPhase
    | DonePhase

//
// --------- Msg ---------
//

type Msg =
    | DealNextCard



//
// --------- Update ---------
//

let updateTableau tableau faceUp card =
    match tableau with down,_ -> if faceUp then [card],down else card::[],down

let update msg model =
    match model with

    | InitPhase initModel -> 
        match msg with
        | DealNextCard ->
            match initModel.deck with
            | [] -> PlayingPhase,Term
            | card :: remaining -> 
                let newPiles,newMoves =
                    match initModel.tableauDealMoves with
                    | move :: tail -> 
                        match move with
                        | One,_ -> { initModel.piles with tableau1 = [card] },tail
                        | Two,faceUp -> { initModel.piles with tableau2 = updateTableau initModel.piles.tableau2 faceUp card },tail
                        | Three,faceUp -> { initModel.piles with tableau3 = updateTableau initModel.piles.tableau3 faceUp card },tail
                        | Four,faceUp -> { initModel.piles with tableau4 = updateTableau initModel.piles.tableau4 faceUp card },tail
                        | Five,faceUp -> { initModel.piles with tableau5 = updateTableau initModel.piles.tableau5 faceUp card },tail
                        | Six,faceUp -> { initModel.piles with tableau6 = updateTableau initModel.piles.tableau6 faceUp card },tail
                        | Seven,faceUp -> { initModel.piles with tableau7 = updateTableau initModel.piles.tableau7 faceUp card },tail
                    | [] -> { initModel.piles with stock = card::initModel.piles.stock },[]
                InitPhase 
                    { initModel with 
                        deck = remaining
                        tableauDealMoves = newMoves
                        piles = newPiles
                        }
                    ,Msg DealNextCard

    | PlayingPhase -> model,Term
    | DonePhase -> model,Term



//
// --------- Init ---------
//

let initCards = 
    [for suit in suits do
        for face in faces do
            yield Card (suit, face)]

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

let initGame rng =
    let deck = 
        initCards
        |> Array.ofList
        |> shuffle rng
        |> List.ofArray
    runGameState update (Msg DealNextCard) (InitPhase { deck = deck; tableauDealMoves = tableauDealMoves; piles = initPiles })