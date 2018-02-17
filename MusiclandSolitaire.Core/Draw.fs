module Draw

open Core
open FsGame
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
    | card :: _ -> 
        [Sprite
          ( getTextures card
          , position
          , 1.0
          , {
            id = TagId.Nothing
            position = position
            tapHandler = 
                optional { 
                    let! f = tapped
                    return! f card
                    }
            touchDownHandler = touchDown
            touchUpHandler = touchUp
            dragHandler = dragged
            stopTouchPropagation = true
            overlapHandler = None
            }
          )
        ]

let cardBack _ = ["CardBack"]

let cardFront (Card ( suit , face ) ) = [getFaceContent face; getSuitContent suit]

let stock model =
    drawPile
        model.stock
        cardBack
        (Point stockPosition)
        (handler PreparePop)
        None
        None
        (Some (fun _ -> if model.popReady then handler PopStock else None))


let faceUpPile cards origin position =
    drawPile
        cards
        cardFront
        position
        (Some (fun id _ -> BeginMove (origin,1,position,id)))
        None
        (Some (fun id _ -> CommitMove id))
        (Some (fun card -> handler (CardTapped card)))

let talon model =
    faceUpPile
        model.talon
        Talon
        (Point talonPosition)

let foundations model =
    faceUpPile (cardsInFoundation model.heartsFoundation) (MoveOrigin.Foundation Hearts) (Point heartsFoundationPosition)
    @ faceUpPile (cardsInFoundation model.spadesFoundation) (MoveOrigin.Foundation Spades) (Point spadesFoundationPosition)
    @ faceUpPile (cardsInFoundation model.diamondsFoundation) (MoveOrigin.Foundation Diamonds) (Point diamondsFoundationPosition)
    @ faceUpPile (cardsInFoundation model.clubsFoundation) (MoveOrigin.Foundation Clubs) (Point clubsFoundationPosition)

let drawFannedPileCard getTextures card tapped touchUp dragged pile (x,y) =
    Sprite
      ( getTextures card
      , Point (x,y)
      , 1.0
      , {
        id = TagId.Nothing
        position = Point (x,y)
        tapHandler = tapped card
        touchDownHandler = None
        touchUpHandler = touchUp pile
        dragHandler = dragged pile (Point (x,y))
        stopTouchPropagation = true
        overlapHandler = None
        }
      ) 

let movingOverlap movingFace = function
| Overlap ({id = TagId.Target (Card (_,face), target); position = point },box) when area box > 4000.0 ->
    StageMove ( face, target, point ) |> Some

| Overlap ({id = TagId.Target (_, target) },box) ->
    UnstageMove target |> Some

| _ -> None


let rec drawFannedPile pile getTextures position dragged touchUp tapped topArea =
    match pile,position with
    | [],_ -> []
    | (card :: []),Point (x,y) -> 
        drawFannedPileCard getTextures card tapped touchUp dragged pile (x,y)
        :: topArea ( Point ( x, y + cardSpacing ) ) card
    | (card :: tail),Point (x,y) -> 
        drawFannedPileCard getTextures card tapped touchUp dragged pile (x,y)
        :: drawFannedPile tail getTextures ( Point ( x, y + cardSpacing ) ) dragged touchUp tapped topArea

let tableau tableau (x,y) model =
    let (Tableau (up, down)) = getTableau tableau model
    drawFannedPile 
        down
        cardBack
        (Point (x,y))
        (fun _ _ -> None)
        (fun _ -> None)
        (fun _ -> None)
        (fun _ _ -> [])
    @ drawFannedPile
        (List.rev up)
        cardFront
        (Point (x,(y + cardSpacing * (float)(List.length down))))
        (fun pile pos -> match (List.length pile),model.moving with x,None when x > 0 -> (Some (fun id _ _ -> BeginMove (MoveOrigin.Tableau tableau,x,pos,id))) | _ -> None)
        (fun pile -> match (List.length pile),model.moving with 1,(Some _) -> Some (fun id _ -> CommitMove id) | _ -> None)
        (CardTapped >> handler)
        (fun pos card -> 
            [Area 
              ( "CardBack"
              , pos
              , { 
                id = TagId.Target ( card , MoveTarget.Tableau tableau )
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
            overlapHandler = bottom |> face |> movingOverlap |> Some
            } 
          )
        :: drawFannedPile
            (List.rev cards)
            cardFront
            position
            (fun _ _ -> None)
            (fun _ -> None)
            (fun _ -> None)
            (fun _ _ -> [])

let draw model =
    background
    :: flipTalon
    :: reset
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model
