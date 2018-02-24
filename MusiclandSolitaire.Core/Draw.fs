module Draw

open Core
open FsGame.Platform
open Model

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


let cardBack = ["CardBack"]


let cardFront (Card ( suit , face ) ) = [getFaceContent face; getSuitContent suit]


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


let drawCard position faceUp tapHandler touchDownHandler touchUpHandler dragHandler card =
  Sprite
    ( if faceUp then cardFront card else cardBack
    , position
    , 1.0
    , {
      id = TagId.Nothing
      position = position
      tapHandler = tapHandler
      touchDownHandler = touchDownHandler
      touchUpHandler = touchUpHandler
      dragHandler = dragHandler
      stopTouchPropagation = true
      overlapHandler = None
      }
    )


let drawPile pile sprite =
    match pile with
    | [] -> []
    | card :: _ -> [sprite card]


let stock model =
    drawCard
        ( Point stockPosition )
        false
        ( if model.popReady then handler PopStock else None )
        ( handler PreparePop )
        None
        None
    |> drawPile model.stock


let faceUpPile hand cards origin position =
    let dragHandler =
        match hand with
        | Some _ -> None
        | None -> ( Some ( fun id _ _ -> BeginMove ( origin , 1 , position , id ) ) )

    drawPile
        cards
        (fun card ->
            drawCard
                position
                true
                ( handler ( CardTapped card ) )
                None
                None
                dragHandler
                card
        )


let talon model =
    faceUpPile
        model.hand
        model.talon
        Talon
        ( Point talonPosition )


let foundations model =
    List.zip 
        [ Hearts; Spades; Diamonds; Clubs ]
        [ heartsFoundationPosition; spadesFoundationPosition; diamondsFoundationPosition; clubsFoundationPosition ]
    |> List.collect (
        fun ( suit , position ) ->
            let position = Point position
            let cards = getFoundation suit model |> cardsInFoundation
            Area 
              ( "CardBack"
              , position
              , { 
                id = TagId.Target ( MoveTarget.Foundation suit )
                position = position
                tapHandler = None
                touchDownHandler = None
                touchUpHandler = None
                dragHandler = None
                stopTouchPropagation = false
                overlapHandler = None
                } 
              )
            :: faceUpPile model.hand cards ( MoveOrigin.Foundation suit ) ( position ) )


let rec drawFannedPile pile position sprite topArea =
    match ( pile , position ) with
    | ( [] , pos ) -> 
        topArea pos

    | ( card :: tail , Point ( x , y ) ) -> 
        sprite ( Point ( x , y ) ) ( card :: tail ) card
        :: drawFannedPile tail ( Point ( x , y + cardSpacing ) ) sprite topArea


let tableau tableau ( x , y ) model =
    let ( Tableau ( up , down ) ) = getTableau tableau model

    drawFannedPile 
        down
        ( Point ( x , y ) )
        ( fun pos _ -> drawCard pos false None None None None )
        ( fun _ -> [] )
    @ drawFannedPile
        (List.rev up)
        ( Point ( x ,  y + cardSpacing * (List.length down |> float) ) )
        (fun pos pile card -> 
            drawCard
                pos
                true
                (CardTapped card |> handler)
                None
                None
                ( match model.hand with
                  | Some _ -> None
                  | None ->
                      let length = List.length pile
                      if length > 0 then
                          Some ( fun id _ _ -> BeginMove ( MoveOrigin.Tableau tableau , length , pos , id ) )
                      else
                          None
                )
                card
        )
        (fun pos -> 
            [Area 
              ( "CardBack"
              , pos
              , {
                id = TagId.Target ( MoveTarget.Tableau tableau )
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



let movingOverlap movingFace = function
| Overlap ( { id = TagId.Target target; position = point } , box ) when area box > 4000.0 ->
    StageMove ( target, point ) |> Some

| Overlap ( { id = TagId.Target target } , box ) ->
    UnstageMove target |> Some

| _ -> None


let moving model =
    match model.hand with
    | None -> []
    | Some ( _ , cards , movingPosition , _ , staging ) ->
        let bottom = List.head cards
        let position =
            match staging with
            | Some ( Unstaging ( origin, progress ) ) -> lerp origin movingPosition progress
            | Some ( Staged ( _, origin, dest, progress , _ , _ ) ) -> lerp origin dest progress
            | None -> movingPosition

        Sprite
          ( cardBack
          , movingPosition
          , 0.5
          , {
            id = TagId.Nothing
            position = position
            tapHandler = None
            touchDownHandler = None
            touchUpHandler = None
            dragHandler = None
            stopTouchPropagation = true
            overlapHandler = None
            }
          )
        :: Area 
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
            ( List.rev cards )
            position
            ( fun pos _ -> drawCard pos true None None None None )
            ( fun _ -> [] )


let draw model =
    background
    :: flipTalon
    :: reset
    :: stock model
    @ talon model
    @ foundations model
    @ tableaus model
    @ moving model
