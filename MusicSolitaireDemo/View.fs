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

let withinBox (x,y) (bx,by) bwidth bheight = x >= bx && y >= by && x < bwidth + bx && y < bheight + by

let handleTouchUp position =
    if withinBox position heartsFoundationPosition 86.0 115.0 then
        CommitMove HeartsFoundation
    else if withinBox position spadesFoundationPosition 86.0 115.0 then
        CommitMove SpadesFoundation
    else if withinBox position diamondsFoundationPosition 86.0 115.0 then
        CommitMove DiamondsFoundation
    else if withinBox position clubsFoundationPosition 86.0 115.0 then
        CommitMove ClubsFoundation
    else if withinBox position tableau1Position 86.0 115.0 then
        CommitMove Tableau1
    else if withinBox position tableau2Position 86.0 115.0 then
        CommitMove Tableau2
    else if withinBox position tableau3Position 86.0 115.0 then
        CommitMove Tableau3
    else if withinBox position tableau4Position 86.0 115.0 then
        CommitMove Tableau4
    else if withinBox position tableau5Position 86.0 115.0 then
        CommitMove Tableau5
    else if withinBox position tableau6Position 86.0 115.0 then
        CommitMove Tableau6
    else if withinBox position tableau7Position 86.0 115.0 then
        CommitMove Tableau7
    else
        CancelMove

let background : Sprite<Msg> = {
    textures = ["Table"]
    position = 0.0,0.0
    touchDown = None
    touchMoved = (Some Move)
    touchUp = (Some handleTouchUp)
}

let drawPile pile getTextures position touchDown touchMoved touchUp =
    match pile with
    | [] -> []
    | (face, suit)::_ -> 
        [{
            textures = getTextures face suit
            position = position
            touchDown = touchDown
            touchMoved = touchMoved
            touchUp = touchUp
        }]

let cardBack _ _ = ["CardBack"]

let cardFront suit face = [getFaceContent face; getSuitContent suit]

let stock model =
    drawPile
        model.stock
        cardBack
        stockPosition
        None
        None
        (Some (fun _ -> PopStock))

let faceUpPile cards pile position =
    drawPile
        cards
        cardFront
        position
        (Some (fun _ -> (BeginMove (pile,1,position))))
        None
        (Some (fun _ -> (CommitMove (pile))))

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

let rec drawFannedPile pile getTextures position touchDown touchUp =
    match pile,position with
    | [],_ -> []
    | ((face, suit) :: tail),(x,y) ->
        {
            textures = getTextures face suit
            position = x,y
            touchDown = touchDown pile position
            touchMoved = None
            touchUp = touchUp pile
        }
        :: (drawFannedPile tail getTextures (x,y+32.0) touchDown touchUp)

let tableau down up tableau position model =
    let x,y = position
    drawFannedPile 
        down
        cardBack
        (x,y)
        (fun _ _ -> None)
        (fun _ -> None)
    @ drawFannedPile
        (List.rev up)
        cardFront
        (x,(y + 32.0 * (float)(List.length down)))
        (fun pile pos -> match (List.length pile),model.moving with x,None when x > 0 -> (Some (fun _ ->BeginMove (tableau,x,pos))) | _ -> None)
        (fun pile -> match (List.length pile),model.moving with 1,(Some _) -> Some (fun _ -> CommitMove (tableau)) | _ -> None)

let tableaus model =
    tableau [] model.tableau1 Tableau1 tableau1Position model 
    @ match model.tableau2 with up,down -> tableau down up Tableau2 tableau2Position model 
    @ match model.tableau3 with up,down -> tableau down up Tableau3 tableau3Position model 
    @ match model.tableau4 with up,down -> tableau down up Tableau4 tableau4Position model 
    @ match model.tableau5 with up,down -> tableau down up Tableau5 tableau5Position model 
    @ match model.tableau6 with up,down -> tableau down up Tableau6 tableau6Position model 
    @ match model.tableau7 with up,down -> tableau down up Tableau7 tableau7Position model 

let moving model =
    match model.moving with
    | None -> []
    | Some (_,cards,position) ->
        drawFannedPile
            (List.rev cards)
            cardFront
            position
            (fun _ _ -> None)
            (fun _ -> None)
        

let view model =
    background
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model