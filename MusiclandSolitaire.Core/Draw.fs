module Draw

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

let withinBox (Point (x,y)) bwidth bheight (bx,by) = x >= bx && y >= by && x < bwidth + bx && y < bheight + by

let handleTouchUp id position =
    let isPositionWithinBox = withinBox position 86.0 115.0
    if isPositionWithinBox heartsFoundationPosition then
        CommitMove (Pile HeartsFoundation,id)
    else if isPositionWithinBox spadesFoundationPosition then
        CommitMove (Pile SpadesFoundation,id)
    else if isPositionWithinBox diamondsFoundationPosition then
        CommitMove (Pile DiamondsFoundation,id)
    else if isPositionWithinBox clubsFoundationPosition then
        CommitMove (Pile ClubsFoundation,id)
    else if isPositionWithinBox tableau1Position then
        CommitMove (Tableau Tableau1,id)
    else if isPositionWithinBox tableau2Position then
        CommitMove (Tableau Tableau2,id)
    else if isPositionWithinBox tableau3Position then
        CommitMove (Tableau Tableau3,id)
    else if isPositionWithinBox tableau4Position then
        CommitMove (Tableau Tableau4,id)
    else if isPositionWithinBox tableau5Position then
        CommitMove (Tableau Tableau5,id)
    else if isPositionWithinBox tableau6Position then
        CommitMove (Tableau Tableau6,id)
    else if isPositionWithinBox tableau7Position then
        CommitMove (Tableau Tableau7,id)
    else
        CancelMove id

let handler msg = Some (fun _ _ -> msg)

let background =
    Sprite 
      ( ["Table"]
      , (Point (0.0, 0.0))
      , 1.0
      , {
        tapHandler = None
        touchDownHandler = None
        touchUpHandler = Some handleTouchUp
        dragHandler = Some (fun id delta _ -> Move (id, delta))
        stopTouchPropagation = true
        overlapHandler = None
        }
      )

let flipTalon =
    Sprite
      ( ["Reset"]
      , (Point (let sx,sy = stockPosition in (sx + 18.0,sy + 30.0)))
      , 0.5
      , {
        tapHandler = handler FlipTalon
        touchDownHandler = None
        touchUpHandler = None
        dragHandler = None
        stopTouchPropagation = true
        overlapHandler = None
        }
      )

let reset =
  Sprite
    ( ["Reset"]
    , (Point (26.0,1252.0))
    , 1.0
    , {
      tapHandler = handler Reset
      touchDownHandler = None
      touchUpHandler = None
      dragHandler = None
      stopTouchPropagation = true
      overlapHandler = None
      }
    )

let drawPile pile getTextures position touchDown dragged touchUp tapped =
    match pile with
    | [] -> []
    | (suit, face)::_ -> 
        [Sprite
          ( getTextures suit face
          , position
          , 1.0
          , {
            tapHandler = 
                optional { 
                    let! f = tapped
                    return! f suit face 
                    }
            touchDownHandler = touchDown
            touchUpHandler = touchUp
            dragHandler = dragged
            stopTouchPropagation = true
            overlapHandler = None
            }
          )
        ]

let cardBack _ _ = ["CardBack"]

let cardFront suit face = [getFaceContent face; getSuitContent suit]

let stock model =
    drawPile
        model.stock
        cardBack
        (Point stockPosition)
        (handler PreparePop)
        None
        None
        (handler (if model.popReady then handler PopStock else None))


let faceUpPile cards pile position =
    drawPile
        cards
        cardFront
        position
        (Some (fun id _ -> BeginMove (Pile pile,1,position,id)))
        None
        (Some (fun id _ -> CommitMove (Pile pile,id)))
        (Some (fun suit face -> handler (CardTapped ((suit,face)))))

let talon model =
    faceUpPile
        model.talon
        Talon
        (Point talonPosition)

let foundations model =
    faceUpPile model.heartsFoundation HeartsFoundation (Point heartsFoundationPosition)
    @ faceUpPile model.spadesFoundation SpadesFoundation (Point spadesFoundationPosition)
    @ faceUpPile model.diamondsFoundation DiamondsFoundation (Point diamondsFoundationPosition)
    @ faceUpPile model.clubsFoundation ClubsFoundation (Point clubsFoundationPosition)

let rec drawFannedPile pile getTextures position touchDown touchUp tapped =
    match pile,position with
    | [],_ -> []
    | ((suit, face) :: tail),Point (x,y) ->
        Sprite
          ( getTextures suit face
          , Point (x,y)
          , 1.0
          , {
            tapHandler = tapped face suit
            touchDownHandler = touchDown pile position
            touchUpHandler = touchUp pile
            dragHandler = None
            stopTouchPropagation = true
            overlapHandler = None
            }
          ) :: (drawFannedPile tail getTextures (Point (x,y+32.0)) touchDown touchUp tapped)

let tableau tableau (x,y) model =
    let (Tableau.Tableau (up, down)) = getTableau tableau model
    drawFannedPile 
        down
        cardBack
        (Point (x,y))
        (fun _ _ -> None)
        (fun _ -> None)
        (fun _ _ -> None)
    @ drawFannedPile
        (List.rev up)
        cardFront
        (Point (x,(y + 32.0 * (float)(List.length down))))
        (fun pile pos -> match (List.length pile),model.moving with x,None when x > 0 -> (Some (fun id _ -> BeginMove (Tableau tableau,x,pos,id))) | _ -> None)
        (fun pile -> match (List.length pile),model.moving with 1,(Some _) -> Some (fun id _ -> CommitMove (Tableau tableau,id)) | _ -> None)
        (fun suit face -> (handler (CardTapped (face,suit))))

let tableaus model =
    tableau Tableau1 tableau1Position model 
    @ tableau Tableau2 tableau2Position model 
    @ tableau Tableau3 tableau3Position model 
    @ tableau Tableau4 tableau4Position model 
    @ tableau Tableau5 tableau5Position model 
    @ tableau Tableau6 tableau6Position model 
    @ tableau Tableau7 tableau7Position model 

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

let draw model =
    background
    :: flipTalon
    :: reset
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model
