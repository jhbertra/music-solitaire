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
        (getFaceContent Do) + "_Sus"
        (getFaceContent Re) + "_Sus"
        (getFaceContent Mi) + "_Sus"
        (getFaceContent Fa) + "_Sus"
        (getFaceContent So) + "_Sus"
        (getFaceContent La) + "_Sus"
        (getFaceContent Ti) + "_Sus"
        (getFaceContent Do8) + "_Sus"
        (getFaceContent IV) + "_Sus"
        (getFaceContent V) + "_Sus"
        (getFaceContent I) + "_Sus"
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

let handleTouchUp position id =
    let isPositionWithinBox = withinBox position 86.0 115.0
    if isPositionWithinBox heartsFoundationPosition then
        Msg (CommitMove (HeartsFoundation,id))
    else if isPositionWithinBox spadesFoundationPosition then
        Msg (CommitMove (SpadesFoundation,id))
    else if isPositionWithinBox diamondsFoundationPosition then
        Msg (CommitMove (DiamondsFoundation,id))
    else if isPositionWithinBox clubsFoundationPosition then
        Msg (CommitMove (ClubsFoundation,id))
    else if isPositionWithinBox tableau1Position then
        Msg (CommitMove (Tableau1,id))
    else if isPositionWithinBox tableau2Position then
        Msg (CommitMove (Tableau2,id))
    else if isPositionWithinBox tableau3Position then
        Msg (CommitMove (Tableau3,id))
    else if isPositionWithinBox tableau4Position then
        Msg (CommitMove (Tableau4,id))
    else if isPositionWithinBox tableau5Position then
        Msg (CommitMove (Tableau5,id))
    else if isPositionWithinBox tableau6Position then
        Msg (CommitMove (Tableau6,id))
    else if isPositionWithinBox tableau7Position then
        Msg (CommitMove (Tableau7,id))
    else
        Msg (TouchDropped id)

let background : Sprite<Msg> = {
    textures = ["Table"]
    position = 0.0,0.0
    touchDown = None
    touchMoved = (Some (fun pos id -> Msg (Move (pos,id))))
    touchUp = (Some handleTouchUp)
    tapped = None
    alpha = 1.0
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
            alpha = 1.0
        }]

let cardBack _ _ = ["CardBack"]

let cardFront suit face = [getFaceContent face; getSuitContent suit]

let stock model =
    drawPile
        model.stock
        cardBack
        stockPosition
        (Some (fun _ -> (fun _ -> Msg PreparePop)))
        None
        (if model.popReady then (Some (fun _ -> (fun _ -> Msg PopStock))) else None)
        (fun _ _ -> None)

let faceUpPile cards pile position =
    drawPile
        cards
        cardFront
        position
        (Some (fun _ -> (fun id -> Msg (BeginMove (pile,1,position,id)))))
        None
        (Some (fun _ -> (fun id -> Msg (CommitMove (pile,id)))))
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
            alpha = 1.0
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
        (fun pile pos -> match (List.length pile),model.moving with x,None when x > 0 -> (Some (fun _ -> (fun id -> Msg (BeginMove (tableau,x,pos,id))))) | _ -> None)
        (fun pile -> match (List.length pile),model.moving with 1,(Some _) -> Some (fun _ -> (fun id -> Msg (CommitMove (tableau,id)))) | _ -> None)
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
    | Some (_,cards,position,_,_) ->
        drawFannedPile
            (List.rev cards)
            cardFront
            position
            (fun _ _ -> None)
            (fun _ -> None)
            (fun _ _ -> None)
        

let view model =
    background
    :: {textures = ["Reset"]; position = (let sx,sy = stockPosition in (sx + 18.0,sy + 30.0)); touchDown = None; touchMoved = None; touchUp = (Some (fun _ -> (fun _ -> Msg FlipStock))); tapped = None; alpha = 0.5}
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model
    @ [{textures = ["Reset"]; position = 26.0,1252.0; touchDown = None; touchMoved = None; touchUp = (Some (fun _ -> (fun _ -> Msg Reset))); tapped = None; alpha = 1.0}]