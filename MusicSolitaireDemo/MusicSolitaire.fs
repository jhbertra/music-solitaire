module MusicSolitaire

// ***************************************** //
//                  CORE                     //
// ***************************************** //

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








// ***************************************** //
//                  GAME                     //
// ***************************************** //

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

type Card = Suit * Face

type TableauNumber =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven

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



//
// --------- Init ---------
//

let initModel rng = {
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
    phase = DealPhase {
        deck = 
            [for suit in suits do for face in faces do yield suit,face]
            |> Array.ofList
            |> shuffleArr rng
            |> List.ofArray
        tableauDealMoves = 
            let numberOrder = [One;Two;Three;Four;Five;Six;Seven]
            numberOrder
            |> List.mapi (fun i _ -> (List.skip i numberOrder) |> List.mapi (fun j num -> num,j=0))
            |> List.collect (fun pass -> pass)
        }
    }



//
// --------- Msg ---------
//

type Msg =
    | DealCards
    | PopStock



//
// --------- Update ---------
//

let updateTableau tableau faceUp card =
    match tableau with up,down -> if faceUp then card::up,down else up,card::down

let update msg model =
    match model.phase with

    | DealPhase dealModel -> 
        match msg with

        | DealCards ->
            match dealModel.deck with
            | [] -> { model with phase = PlayingPhase },Term
            | card :: remainingDeck -> 
                match dealModel.tableauDealMoves with
                | move :: remainingMoves -> 
                    match move with
                    | One,_ -> { model with tableau1 = [card]; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                    | Two,faceUp -> { model with tableau2 = updateTableau model.tableau2 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                    | Three,faceUp -> { model with tableau3 = updateTableau model.tableau3 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                    | Four,faceUp -> { model with tableau4 = updateTableau model.tableau4 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                    | Five,faceUp -> { model with tableau5 = updateTableau model.tableau5 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                    | Six,faceUp -> { model with tableau6 = updateTableau model.tableau6 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                    | Seven,faceUp -> { model with tableau7 = updateTableau model.tableau7 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | [] -> { model with stock = card :: model.stock; phase = DealPhase { deck = remainingDeck; tableauDealMoves = [] } }
                ,Msg DealCards

        | _ -> model,Term

    | PlayingPhase -> 
        match msg with

        | PopStock ->
            match model.stock with
            | [] -> model,Term
            | head::tail -> 
                { model with
                    stock = tail
                    talon = head :: model.talon
                }
                ,Term
        
        | _ -> model,Term

    | DonePhase -> model,Term








// ***************************************** //
//                PLATFORM                   //
// ***************************************** //

//
// --------- Cmd --------
//

let rec runCmd cmd = 
    state {
        let! model = getState
        match cmd with
        | Term -> return model
        | Msg msg -> 
            let newModel,nextCmd = update msg model
            do! putState newModel
            return! runCmd nextCmd
        }

let execGameState cmd model =
    let gameStateBuilder = runCmd cmd
    execState gameStateBuilder model



//
// --------- Game --------
//

open Microsoft.Xna.Framework

type MusicSolitaireGame() as game =
    inherit Game() 
    let manager = new GraphicsDeviceManager(game)
    override __.Draw _ = game.GraphicsDevice.Clear Color.CornflowerBlue