module View

open Core
open Model
open Update

//
// --------- Content Declarations ---------
//

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
        "Reset"
        ]
    sfx =
        [
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
        "Stack"
        ]
}



//
// --------- View ---------
//

// Top Row
let heartsFoundationPosition = 26.0,56.0
let spadesFoundationPosition = 128.0,56.0
let diamondsFoundationPosition = 230.0,56.0
let clubsFoundationPosition = 332.0,56.0
let talonPosition = 536.0,56.0
let stockPosition = 638.0,56.0

// Bottom Row
let tableau1Position = 26.0,223.0
let tableau2Position = 128.0,223.0
let tableau3Position = 230.0,223.0
let tableau4Position = 332.0,223.0
let tableau5Position = 434.0,223.0
let tableau6Position = 536.0,223.0
let tableau7Position = 638.0,223.0

let withinBox (x,y) bwidth bheight (bx,by) = x >= bx && y >= by && x < bwidth + bx && y < bheight + by

let handleTouchUp position =
    let isPositionWithinBox = withinBox position 86.0 115.0
    if isPositionWithinBox heartsFoundationPosition then
        CommitMove HeartsFoundation
    else if isPositionWithinBox spadesFoundationPosition then
        CommitMove SpadesFoundation
    else if isPositionWithinBox diamondsFoundationPosition then
        CommitMove DiamondsFoundation
    else if isPositionWithinBox clubsFoundationPosition then
        CommitMove ClubsFoundation
    else if isPositionWithinBox tableau1Position then
        CommitMove Tableau1
    else if isPositionWithinBox tableau2Position then
        CommitMove Tableau2
    else if isPositionWithinBox tableau3Position then
        CommitMove Tableau3
    else if isPositionWithinBox tableau4Position then
        CommitMove Tableau4
    else if isPositionWithinBox tableau5Position then
        CommitMove Tableau5
    else if isPositionWithinBox tableau6Position then
        CommitMove Tableau6
    else if isPositionWithinBox tableau7Position then
        CommitMove Tableau7
    else
        CancelMove

let background : Sprite<Msg> = {
    textures = ["Table"]
    position = 0.0,0.0
    touchDown = None
    touchMoved = (Some Move)
    touchUp = (Some handleTouchUp)
    tapped = None
}

let drawPile pile getTextures position touchDown touchMoved touchUp tapped =
    match pile with
    | [] -> []
    | (suit, face)::_ -> 
        [{
            textures = getTextures suit face
            position = position
            touchDown = touchDown
            touchMoved = touchMoved
            touchUp = touchUp
            tapped = tapped suit face
        }]

let cardBack _ _ = ["CardBack"]

let cardFront suit face = [getFaceContent face; getSuitContent suit]

let stock model =
    drawPile
        model.stock
        cardBack
        stockPosition
        (Some (fun _ -> PreparePop))
        None
        (if model.popReady then (Some (fun _ -> PopStock)) else None)
        (fun _ _ -> None)

let faceUpPile cards pile position =
    drawPile
        cards
        cardFront
        position
        (Some (fun _ -> (BeginMove (pile,1,position))))
        None
        (Some (fun _ -> (CommitMove (pile))))
        (fun suit face -> (suit,face) |> CardTapped |> Some)

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

let rec drawFannedPile pile getTextures position touchDown touchUp tapped =
    match pile,position with
    | [],_ -> []
    | ((suit, face) :: tail),(x,y) ->
        {
            textures = getTextures suit face
            position = x,y
            touchDown = touchDown pile position
            touchMoved = None
            touchUp = touchUp pile
            tapped = tapped face suit
        }
        :: (drawFannedPile tail getTextures (x,y+32.0) touchDown touchUp tapped)

let tableau down up tableau position model =
    let x,y = position
    drawFannedPile 
        down
        cardBack
        (x,y)
        (fun _ _ -> None)
        (fun _ -> None)
        (fun _ _ -> None)
    @ drawFannedPile
        (List.rev up)
        cardFront
        (x,(y + 32.0 * (float)(List.length down)))
        (fun pile pos -> match (List.length pile),model.moving with x,None when x > 0 -> (Some (fun _ ->BeginMove (tableau,x,pos))) | _ -> None)
        (fun pile -> match (List.length pile),model.moving with 1,(Some _) -> Some (fun _ -> CommitMove (tableau)) | _ -> None)
        (fun suit face -> (face,suit) |> CardTapped |> Some)

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
            (fun _ _ -> None)
        

let view model =
    background
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model
    @ [{textures = ["Reset"]; position = 26.0,1252.0; touchDown = None; touchMoved = None; touchUp = (Some (fun _ -> Reset)); tapped = None}]