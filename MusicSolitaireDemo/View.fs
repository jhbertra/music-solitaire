module View

open Core
open Model
open Update

//
// --------- Content Declarations ---------
//

let getFaceContent face =
    match face with
        | KeySignature -> "Ks"
        | Do -> "Do"
        | Re -> "Re"
        | Mi -> "Mi"
        | Fa -> "Fa"
        | So -> "So"
        | La -> "La"
        | Ti -> "Ti"
        | Do8 -> "Do8"
        | IV -> "IV"
        | V -> "V"
        | I -> "I"

let getSuitContent suit =
    match suit with
        | Hearts -> "Hearts"
        | Spades -> "Spades"
        | Diamonds -> "Diamonds"
        | Clubs -> "Clubs"

let contentManifest = {
    root = "Content"
    textures = 
        [
        "Table"
        "CardBack"
        getFaceContent KeySignature
        getFaceContent Do
        getFaceContent Re
        getFaceContent Mi
        getFaceContent Fa
        getFaceContent So
        getFaceContent La
        getFaceContent Ti
        getFaceContent Do8
        getFaceContent IV
        getFaceContent V
        getFaceContent I
        getSuitContent Hearts
        getSuitContent Spades
        getSuitContent Diamonds
        getSuitContent Clubs
        ]
}



//
// --------- View ---------
//

// Top Row
let heartsFoundationPosition = 26.0,26.0
let spadesFoundationPosition = 128.0,26.0
let diamondsFoundationPosition = 230.0,26.0
let clubsFoundationPosition = 332.0,26.0
let talonPosition = 536.0,26.0
let stockPosition = 638.0,26.0

// Bottom Row
let tableau1Position = 26.0,193.0
let tableau2Position = 128.0,193.0
let tableau3Position = 230.0,193.0
let tableau4Position = 332.0,193.0
let tableau5Position = 434.0,193.0
let tableau6Position = 536.0,193.0
let tableau7Position = 638.0,193.0

let background = {
    textures = ["Table"]
    position = 0.0,0.0
    tap = None
    touchDown = None
    touchUp = None
}

let drawPile pile getTextures position tap touchDown touchUp =
    match pile with
    | [] -> []
    | (face, suit)::_ -> 
        [{
            textures = getTextures face suit
            position = position
            tap = tap
            touchDown = touchDown
            touchUp = touchUp
        }]

let cardBack _ _ = ["CardBack"]

let cardFront suit face = [getFaceContent face; getSuitContent suit]

let stock model =
    drawPile
        model.stock
        cardBack
        stockPosition
        (Some PopStock)
        None
        None

let faceUpPile cards pile position =
    drawPile
        cards
        cardFront
        position
        None
        (Some (BeginMove (pile,1,position)))
        (Some (CommitMove (pile)))

let talon model =
    faceUpPile
        model.talon
        Talon
        talonPosition

let foundations model =
    faceUpPile model.heartsFoundation HeartsFoundation heartsFoundationPosition
    @ faceUpPile model.spadesFoundation SpadesFoundation spadesFoundationPosition
    @ faceUpPile model.diamondsFoundation DiamondsFoundation diamondsFoundationPosition
    @ faceUpPile model.clubsFoundation ClubsFoundation clubsFoundationPosition

let rec drawFannedPile pile getTextures position tap touchDown touchUp =
    match pile,position with
    | [],_ -> []
    | ((face, suit) :: tail),(x,y) ->
        {
            textures = getTextures face suit
            position = x,y
            tap = tap
            touchDown = touchDown pile
            touchUp = touchUp pile
        }
        :: (drawFannedPile tail getTextures (x,y+32.0) tap touchDown touchUp)

let tableau down up tableau position =
    let x,y = position
    drawFannedPile 
        down
        cardBack
        (x,y)
        None
        (fun _ -> None)
        (fun _ -> None)
    @ drawFannedPile
        up
        cardFront
        (x,(y + 32.0 * (float)(List.length down)))
        None
        (fun pile -> match List.length pile with x when x > 0 -> (Some (BeginMove (tableau,x,position))) | _ -> None)
        (fun pile -> match List.length pile with 1 -> Some (CommitMove (tableau)) | _ -> None)

let tableaus model =
    tableau [] model.tableau1 Tableau1 tableau1Position
    @ match model.tableau2 with up,down -> tableau down up Tableau2 tableau2Position
    @ match model.tableau3 with up,down -> tableau down up Tableau3 tableau3Position
    @ match model.tableau4 with up,down -> tableau down up Tableau4 tableau4Position
    @ match model.tableau5 with up,down -> tableau down up Tableau5 tableau5Position
    @ match model.tableau6 with up,down -> tableau down up Tableau6 tableau6Position
    @ match model.tableau7 with up,down -> tableau down up Tableau7 tableau7Position

let moving model =
    match model.moving with
    | None -> []
    | Some (_,cards,position) ->
        drawFannedPile
            cards
            cardFront
            position
            None
            (fun _ -> None)
            (fun _ -> None)
        

let view model =
    background
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model