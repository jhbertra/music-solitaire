module Update

open Core
open Touch
open Model
open FsGame


//
// --------- Init ---------
//

let rec deal cards =
    State.builder {
        for card in cards do
            let! tableauDealMoves = State.gets getTableauDealMoves
            let! model = State.gets getModel

            match tableauDealMoves with
            | [] ->
                do! model |> pushCardToStock card |> setModel |> State.modify

            | (tableau, faceUp) :: moves ->
                do! setTableauDealMoves moves |> State.modify
                do! model |> pushCardToTableau tableau faceUp card |> setModel |> State.modify

        return! State.gets getModel
    }

let initModel rng = 

    let model = {
        won = false
        stock = []
        talon = []
        tableau1 = initTableau
        tableau2 = initTableau
        tableau3 = initTableau
        tableau4 = initTableau
        tableau5 = initTableau
        tableau6 = initTableau
        tableau7 = initTableau
        heartsFoundation = initFoundation Hearts
        spadesFoundation = initFoundation Spades
        diamondsFoundation = initFoundation Diamonds
        clubsFoundation = initFoundation Clubs
        moving = None
        rng = rng
        popReady = false
        pendingGestures = []
        previousTouches = []
        }

    let deck = 
        [ for suit in suits do for face in faces do yield Card ( suit , face ) ]
        |> Array.ofList
        |> shuffleArr rng
        |> List.ofArray

    let dealingState = {
        tableauDealMoves = 
            let numberOrder = [1..7]
            numberOrder |> List.collect (fun i -> (List.skip (i-1) numberOrder) |> List.mapi (fun j num -> tableauNumber num,j=0))
        model = model
        }

    State.eval dealingState (deal deck)

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

//
// --------- Msg ---------
//

type TagId =
    | Nothing
    | Background
    | FlipTalon
    | Reset
    | MovingBottom of Card
    | Target of Card * MoveTarget

type Msg =
    | Step
    | PreparePop
    | PopStock
    | BeginMove of MoveOrigin * int * Point * int
    | Move of int * Delta
    | CancelMove of int
    | StageMove of Face * MoveTarget * Point
    | UnstageMove of MoveTarget
    | CommitMove of int
    | MoveCommitted
    | Reset
    | CardTapped of Card
    | FlipTalon

type Tag = {
    id : TagId
    position : Point
    tapHandler : (int -> Point -> Msg) option
    touchDownHandler : (int -> Point -> Msg) option
    touchUpHandler : (int -> Point -> Msg) option
    dragHandler : (int -> Delta -> Point -> Msg) option
    stopTouchPropagation : bool
    overlapHandler : (Overlap -> Msg option) option
}
and Overlap = Overlap of Tag * BoundingBox

type Operation =
    | Msg of Msg
    | Cmd of Cmd<Model, Tag>



//
// --------- Update ---------
//

let pointInBox (Point (x,y)) (BoundingBox (xb,yb,w,h)) =
    let xpb = x - xb
    let ypb = y - yb
    if xpb >= 0.0 && xpb <= w && ypb >= 0.0 && ypb <= h then
        Some (Point (xpb,ypb))
    else
        None

let rec messagesFromGesture objects (id,gestureType) =
    match objects with
    | [] -> []
    | {tag = tag; box = box}::os ->
        let handler,hitPos =
            match gestureType with
            | GestureType.Tap pos -> tag.tapHandler <?> id, pos
            | GestureType.TouchDown pos -> tag.touchDownHandler <?> id, pos
            | GestureType.TouchUp pos -> tag.touchUpHandler <?> id, pos
            | GestureType.Drag (startPos,delta) -> tag.dragHandler <?> id <?> delta, startPos
        
        let result =
            optional {
                let! handler = handler
                let! pointInBox = pointInBox hitPos box
                return handler pointInBox
            }

        match result with
        | None -> messagesFromGesture os (id,gestureType)
        | Some message ->
            if tag.stopTouchPropagation then
                [message]
            else
                message :: messagesFromGesture os (id,gestureType)

let rec overlaps id box = function
| { box = box2; tag = tag2 } :: os when id <> tag2.id ->
    (intersect box box2
    |> mapOption (fun overlap -> Overlap (tag2,overlap))
    |> optionToSingletonList)
    @ overlaps id box os
| _ -> []

let rec messagesFromObjects = function
| [] -> []
| { tag = { id = id; overlapHandler = Some handler }; box = box } :: os ->
    overlaps id box os
    |> List.map handler
    |> values
    |> List.append (messagesFromObjects os)
| _ :: os -> messagesFromObjects os

let messages objects gestures gameTime = Step :: (gestures |> List.collect (messagesFromGesture objects)) @ messagesFromObjects objects

let setModel model (UpdateResult (_,cmds)) = UpdateResult (model,cmds)

let addCommand command (UpdateResult (model,cmds)) = UpdateResult (model,command :: cmds)

let getModel = 
    State.builder { 
        let! UpdateResult (model,_) = State.get
        return model 
    }

let rec sendMessages processMessage gameTime = function
| [] -> State.get
| msg::msgs ->

    let rec messages = function
    | [] -> []
    | (Cmd _) :: ops -> messages ops
    | (Msg msg) :: ops -> msg :: messages ops

    let rec commands = function
    | [] -> []
    | (Msg _) :: ops -> commands ops
    | (Cmd cmd) :: ops -> cmd :: commands ops

    State.builder {
        let! model = getModel
        let model,nextOperations = processMessage msg model gameTime
        do! State.modify (setModel model)
        for cmd in commands nextOperations do
            do! State.modify (addCommand cmd)
        return! sendMessages processMessage gameTime ((messages nextOperations) @ msgs)
    }

let returnModel model = model,[]

let wrapOperation processMessage : Operation -> Update<Model, Tag> = function
| Msg msg -> fun {model=model; gameTime = gameTime} -> sendMessages processMessage gameTime [msg] |> State.eval (UpdateResult (returnModel model))
| Cmd cmd -> fun {model=model} -> UpdateResult (model, [cmd])

let rec processMessage msg model gameTime =
    if model.won && msg <> Reset then
        returnModel model
    else
        match msg with


        | Step -> 

            let move gameTime progress = 
                let speed = (10.0 - 30.0) * progress + 30.0
                let step = ((float gameTime.elapsed.Milliseconds) * 0.001) * speed
                min 1.0 (progress + step)

            match model.moving with
            | Some ( o , c , p , i , Some ( Unstaging ( origin , progress ) ) ) ->
                let progress = move gameTime progress
                let staging = 
                    if progress = 1.0 then 
                          None 
                      else 
                          Some ( Unstaging ( origin, progress ) )

                returnModel 
                    { model with 
                        moving = 
                            Some 
                              ( o 
                              , c 
                              , p 
                              , i 
                              , staging 
                              )
                    }

            | Some ( o , c , p , i , Some ( Staged ( target, origin, dest, progress ) ) ) ->
                let progress = move gameTime progress

                returnModel 
                    { model with 
                        moving = 
                            Some 
                              ( o 
                              , c 
                              , p 
                              , i 
                              , Some ( Staged ( target, origin, dest, progress ) ) 
                              )
                    }

            | _ -> returnModel model


        | Reset -> initModel model.rng |> returnModel


        | PreparePop -> returnModel { model with popReady = true }


        | PopStock ->

            match (model.popReady),(model.stock) with
            | false,_ -> model

            | true,[] -> model

            | true,(head::tail) -> 
                { model with
                    stock = tail
                    talon = head :: model.talon
                    popReady = false
                }

            |> returnModel


        | BeginMove (origin, count, pos, id) ->
            if model.moving = None then
                let pileSound = List.head >> snd >> getFaceContent >> Some
                let tableauSound = (fun n -> List.take n >> List.last >> snd >> getFaceContent >> Some)
                
                let cardsInPile, pickCards, moving =
                    match ( model.moving , origin , count ) with
                    | None , MoveOrigin.Talon , 1 ->
                        let pickCards = (fun model cards -> { model with talon = cards })
                        model.talon, pickCards, Some ( origin , List.take 1 model.talon , pos , id , None ) 

                    | None , MoveOrigin.Foundation foundation , 1 ->
                        let cardsInFoundation = getFoundation foundation model |> cardsInFoundation
                        let pickCards = flip (setFoundation foundation)
                        cardsInFoundation, pickCards, Some ( origin , List.take 1 cardsInFoundation , pos , id , None ) 

                    | None, MoveOrigin.Tableau tableau, n when n <= (getTableau tableau model |> faceUp |> List.length) && n > 0 ->
                        let cardsInTableau = getTableau tableau model |> faceUp
                        let pickCards = setTableauFaceUp tableau
                        cardsInTableau, pickCards, Some ( origin , List.take n cardsInTableau , pos , id , None ) 

                    | _ -> ( [] , idFunc2 , None )

                returnModel { pickCards model (List.skip count cardsInPile) with moving = moving }

            else
                returnModel model


        | Move (tid, Delta (x,y)) ->
            match model.moving with
            | Some ( origin , cards , Point ( oldX , oldY ) , id , staging ) when id = tid ->
                { model with
                    moving = Some ( origin , cards , Point ( oldX + x , oldY + y ) , id , staging )
                    }
                    ,[]

            | _ -> returnModel model


        | CancelMove tid ->

            let returnCardsToOrigin =
                match model.moving with
                | Some ( MoveOrigin.Talon , cards , _ , id , _ ) when id = tid -> 
                    (@) cards |> modifyTalon

                | Some ( MoveOrigin.Foundation foundation , cards , _ , id , _ ) when id = tid -> 
                    (@) cards |> modifyFoundation foundation 

                | Some ( MoveOrigin.Tableau tableau , cards , _ , id , _ ) when id = tid -> 
                    (@) cards |> modifyTableauFaceUp tableau 

                | _ -> idFunc

            returnModel { returnCardsToOrigin model with moving = None }


        | StageMove ( face, target, point ) ->
            match model.moving with
            | Some ( origin , cards , movingPos , id , None ) -> 
                let movingFace = List.last cards |> Model.face

                ( { model with moving = Some ( origin , cards , movingPos , id , Some ( Staged ( target, movingPos, point, 0.0 ) ) ) }
                , [ PlaySound ( getFaceContent face, (if face = KeySignature then NoOverlap else SoundMode.Overlap), 1.0 )
                    Delay ( 0.33, PlaySound ( getFaceContent movingFace, (if movingFace = KeySignature then NoOverlap else SoundMode.Overlap), 1.0 ) |> Cmd |> wrapOperation processMessage )
                  ] 
                  |> List.map Cmd
                )

            | _ -> returnModel model


        | UnstageMove target ->
            match model.moving with
            | Some ( o , c , p , i , Some ( Staged (  moveTarget, origin, dest, progress ) ) ) when target = moveTarget -> 
                returnModel { model with moving = Some ( o , c , p , i , Some ( Unstaging ( lerp origin dest progress, 0.0 ) ) ) }

            | _ -> returnModel model


        | CommitMove tid ->
            match model.moving with
            | Some ( _ , cards , _ , id , Some ( Staged ( target, _, _, _ ) ) ) when tid = id ->

                match cards,target with

                | [card] , MoveTarget.Foundation foundation when canPlaceOnFoundation (getFoundation foundation model) card ->
                    let face = face card
                    pushCardToFoundation foundation card model,
                        [ 
                            Cmd (PlaySound ((getFaceContent face),(if face = KeySignature then NoOverlap else SoundMode.Overlap),1.0))
                            Msg MoveCommitted
                        ]
                        
                | cards , MoveTarget.Tableau tableau when canPlaceOnTableau (getTableau tableau model |> faceUp) cards ->
                    modifyTableauFaceUp tableau ((@) cards) model, [Msg MoveCommitted]

                | _ -> model,[Msg (CancelMove id)]
            
            | Some ( _ , _ , _ , id , None ) -> model,[Msg (CancelMove id)]

            | _ -> returnModel model


        | MoveCommitted ->
            let model = { model with  moving = None }

            if suits |> List.map ((flip getFoundation) model) |> List.map cardsInFoundation |> List.map List.length = [12;12;12;12]
            then
                { model with won = true }, [Cmd (Delay (1.0,wrapOperation processMessage (Cmd (PlaySound ("Stack",NoOverlap,1.0)))))]
            else 
                model
                |> modifyTableau Tableau2 replenish
                |> modifyTableau Tableau3 replenish
                |> modifyTableau Tableau4 replenish
                |> modifyTableau Tableau5 replenish
                |> modifyTableau Tableau6 replenish
                |> modifyTableau Tableau7 replenish
                |> returnModel


        | CardTapped ( Card ( _ , face ) ) -> model,[Cmd (PlaySound (getFaceContent face, (if face = KeySignature then NoOverlap else SoundMode.Overlap), 1.0))]


        | FlipTalon -> 
            returnModel
                { model with
                    talon = []
                    stock = List.rev model.talon }


let update (gameState : GameState<Model, Tag>) : UpdateResult<Model, Tag> =

    printfn "%f" (1.0 / ((float gameState.gameTime.elapsed.Milliseconds) * 0.001))

    let gestureResults =
        touchEvents gameState.model.previousTouches gameState.touches gameState.gameTime
        |> processEvents gameState.model.pendingGestures

    let model = 
        { gameState.model with
            pendingGestures = pendingGestures gestureResults
            previousTouches = gameState.touches
            }

    messages (List.rev gameState.objects) (gestures gestureResults) gameState.gameTime 
    |> sendMessages processMessage gameState.gameTime
    |> State.eval (UpdateResult (returnModel model))
