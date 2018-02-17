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

let cardSpacing = 32.0

let withinBox (Point (x,y)) bwidth bheight (bx,by) = x >= bx && y >= by && x < bwidth + bx && y < bheight + by

let handleTouchUp id _ = CommitMove id

let handler msg = Some (fun _ _ -> msg)

let background =
    let position = Point (0.0, 0.0)
    Sprite 
      ( ["Table"]
      , position
      , 1.0
      , {
        id = TagId.Background
        position = position
        tapHandler = None
        touchDownHandler = None
        touchUpHandler = Some handleTouchUp
        dragHandler = Some (fun id delta _ -> Move (id, delta))
        stopTouchPropagation = true
        overlapHandler = None
        }
      )

let flipTalon =
    let position = Point (let sx,sy = stockPosition in (sx + 18.0,sy + 30.0))
    Sprite
      ( ["Reset"]
      , position
      , 0.5
      , {
        id = TagId.FlipTalon
        position = position
        tapHandler = handler FlipTalon
        touchDownHandler = None
        touchUpHandler = None
        dragHandler = None
        stopTouchPropagation = true
        overlapHandler = None
        }
      )

let reset =
    let position = Point (26.0,1252.0)
    Sprite
      ( ["Reset"]
      , position
      , 1.0
      , {
        id = TagId.Reset
        position = position
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
            id = TagId.Card (suit, face)
            position = position
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
        (Some (fun id _ -> CommitMove id))
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

let drawFannedPileCard getTextures suit face tapped touchUp dragged pile (x,y) =
    Sprite
      ( getTextures suit face
      , Point (x,y)
      , 1.0
      , {
        id = TagId.Card (suit, face)
        position = Point (x,y)
        tapHandler = tapped face suit
        touchDownHandler = None
        touchUpHandler = touchUp pile
        dragHandler = dragged pile (Point (x,y))
        stopTouchPropagation = true
        overlapHandler = None
        }
      ) 

let movingOverlap movingFace = function
| Overlap ({id = TagId.Target ((_,face), target); position = point },box) when area box > 4000.0 ->
    StageMove ( face, target, point ) |> Some

| Overlap ({id = TagId.Target (_, target) },box) ->
    UnstageMove target |> Some

| _ -> None


let rec drawFannedPile pile getTextures position dragged touchUp tapped topArea =
    match pile,position with
    | [],_ -> []
    | ((suit, face) :: []),Point (x,y) -> 
        drawFannedPileCard getTextures suit face tapped touchUp dragged pile (x,y)
        :: topArea ( Point ( x, y + cardSpacing ) ) suit face
    | ((suit, face) :: tail),Point (x,y) -> 
        drawFannedPileCard getTextures suit face tapped touchUp dragged pile (x,y)
        :: drawFannedPile tail getTextures ( Point ( x, y + cardSpacing ) ) dragged touchUp tapped topArea

let tableau tableau (x,y) model =
    let (Tableau.Tableau (up, down)) = getTableau tableau model
    drawFannedPile 
        down
        cardBack
        (Point (x,y))
        (fun _ _ -> None)
        (fun _ -> None)
        (fun _ _ -> None)
        (fun _ _ _ -> [])
    @ drawFannedPile
        (List.rev up)
        cardFront
        (Point (x,(y + cardSpacing * (float)(List.length down))))
        (fun pile pos -> match (List.length pile),model.moving with x,None when x > 0 -> (Some (fun id _ _ -> BeginMove (Tableau tableau,x,pos,id))) | _ -> None)
        (fun pile -> match (List.length pile),model.moving with 1,(Some _) -> Some (fun id _ -> CommitMove id) | _ -> None)
        (fun suit face -> (handler (CardTapped (face,suit))))
        (fun pos suit face -> 
            [Area 
              ( "CardBack"
              , pos
              , { 
                id = TagId.Target ( ( suit, face ), Tableau tableau )
                position = pos
                tapHandler = None
                touchDownHandler = None
                touchUpHandler = None
                dragHandler = None
                stopTouchPropagation = false
                overlapHandler = None
                } 
              )
            ] 
        )

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
    | Some (_,cards,movingPosition,_) ->
        let bottom = List.head cards
        let position =
            match model.pendingMove, model.unstaging with
            | _, Some ( UnstagingModel ( origin, progress ) ) -> lerp origin movingPosition progress
            | Some ( MoveModel ( _, origin, dest, progress ) ), _ -> lerp origin dest progress
            | _ -> movingPosition
        Area 
          ( "CardBack"
          , movingPosition
          , { 
            id = TagId.MovingBottom bottom
            position = movingPosition
            tapHandler = None
            touchDownHandler = None
            touchUpHandler = None
            dragHandler = None
            stopTouchPropagation = false
            overlapHandler = bottom |> fst |> movingOverlap |> Some
            } 
          )
        :: drawFannedPile
            (List.rev cards)
            cardFront
            position
            (fun _ _ -> None)
            (fun _ -> None)
            (fun _ _ -> None)
            (fun _ _ _ -> [])

let draw model =
    background
    :: flipTalon
    :: reset
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model
