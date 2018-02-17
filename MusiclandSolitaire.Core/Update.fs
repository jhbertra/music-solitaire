module Update

open Core
open Touch
open Model



//
// --------- Init ---------
//

let rec deal cards =
    state {
        for card in cards do
            let! tableauDealMoves = gets getTableauDealMoves
            let! model = gets getModel

            match tableauDealMoves with
            | [] ->
                do! model |> pushCardToPile Stock card |> setModel |> modify

            | (tableau, faceUp) :: moves ->
                do! setTableauDealMoves moves |> modify
                do! model |> pushCardToTableau tableau faceUp card |> setModel |> modify

        return! gets getModel
    }

let initModel rng = 

    let model = {
        won = false
        stock = initPile
        talon = initPile
        tableau1 = initTableau
        tableau2 = initTableau
        tableau3 = initTableau
        tableau4 = initTableau
        tableau5 = initTableau
        tableau6 = initTableau
        tableau7 = initTableau
        heartsFoundation = initPile
        spadesFoundation = initPile
        diamondsFoundation = initPile
        clubsFoundation = initPile
        moving = None
        rng = rng
        popReady = false
        pendingGestures = []
        previousTouches = []
        }

    let deck = 
        [for suit in suits do for face in faces do yield suit,face]
        |> Array.ofList
        |> shuffleArr rng
        |> List.ofArray

    let dealingState = {
        tableauDealMoves = 
            let numberOrder = [1..7]
            numberOrder |> List.collect (fun i -> (List.skip (i-1) numberOrder) |> List.mapi (fun j num -> tableauNumber num,j=0))
        model = model
        }

    evalState dealingState (deal deck)

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
    | Background
    | FlipTalon
    | Reset
    | Card of Card

type Msg =
    | PreparePop
    | PopStock
    | BeginMove of MoveOrigin * int * Point * int
    | Move of int * Delta
    | CancelMove of int
    | CommitMove of MoveOrigin * int
    | MoveCommitted
    | Reset
    | CardTapped of Card
    | FlipTalon
    | HandleMovingSound

type Tag = {
    id : TagId
    tapHandler : (int -> Point -> Msg) option
    touchDownHandler : (int -> Point -> Msg) option
    touchUpHandler : (int -> Point -> Msg) option
    dragHandler : (int -> Delta -> Point -> Msg) option
    stopTouchPropagation : bool
    overlapHandler : (Overlap -> Msg) option
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

let rec overlaps box = function
| [] -> []
| {box = box2; tag = tag} :: os ->
    (intersect box box2
    |> mapOption (fun overlap -> Overlap (tag,overlap))
    |> optionToSingletonList)
    @ overlaps box os

let rec messagesFromObjects = function
| [] -> []
| {tag=tag; box=box} :: os ->
    let overlaps = overlaps box os
    (optional {
        let! handler = tag.overlapHandler
        return overlaps |> List.map handler
    } |> defaultIfNone [])
    @ messagesFromObjects os

let messages objects gestures = (gestures |> List.collect (messagesFromGesture objects)) @ messagesFromObjects objects

let setModel model (UpdateResult (_,cmds)) = UpdateResult (model,cmds)

let addCommand command (UpdateResult (model,cmds)) = UpdateResult (model,command :: cmds)

let getModel = 
    state { 
        let! UpdateResult (model,_) = getState
        return model 
    }

let rec sendMessages processMessage = function
| [] -> getState
| msg::msgs ->

    let rec messages = function
    | [] -> []
    | (Cmd _) :: ops -> messages ops
    | (Msg msg) :: ops -> msg :: messages ops

    let rec commands = function
    | [] -> []
    | (Msg _) :: ops -> commands ops
    | (Cmd cmd) :: ops -> cmd :: commands ops

    state {
        let! model = getModel
        let model,nextOperations = processMessage msg model
        do! modify (setModel model)
        for cmd in commands nextOperations do
            do! modify (addCommand cmd)
        return! sendMessages processMessage ((messages nextOperations) @ msgs)
    }

let returnModel model = model,[]

let wrapOperation processMessage : Operation -> Update<Model, Tag> = function
| Msg msg -> fun {model=model} -> sendMessages processMessage [msg] |> evalState (UpdateResult (returnModel model))
| Cmd cmd -> fun {model=model} -> UpdateResult (model, [cmd])

let rec processMessage msg model =
    if model.won && msg <> Reset then
        returnModel model
    else
        printfn "Msg %A" msg
        match msg with

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
                let susSound = Option.map (fun s -> s + "_Sus")
                
                let cardsInPile, pickCards, sound, moving =
                    match model.moving,origin,count with
                    | None, Pile pile, 1 when pile <> Stock ->
                        let cardsInPile = getPile pile model
                        let pickCards = flip (setPile pile)
                        let sound = pileSound cardsInPile
                        cardsInPile, pickCards, sound, Some (origin,(List.take 1 cardsInPile),pos,susSound sound, id) 

                    | None, Tableau tableau, n when n <= (getTableau tableau model |> faceUp |> List.length) && n > 0 ->
                        let cardsInTableau = getTableau tableau model |> faceUp
                        let pickCards = setTableauFaceUp tableau
                        let sound = tableauSound n cardsInTableau
                        cardsInTableau, pickCards, sound, Some (origin,(List.take n cardsInTableau),pos,susSound sound, id) 

                    | _ -> [], idFunc2, None, None

                let model = { pickCards model (List.skip count cardsInPile) with moving = moving }

                match sound with
                | None -> returnModel model
                | (Some sound) -> 
                    model,
                    [
                        Cmd (PlaySound (sound + "_Sus",SoundMode.Overlap,0.5))
                        Cmd (Delay (1.0, wrapOperation processMessage (Msg HandleMovingSound)))
                        Msg HandleMovingSound
                    ]
            else
                returnModel model

        | Move (tid, Delta (x,y)) ->
            match model.moving with
            | Some (origin,cards,Point (oldX, oldY),s,id) when id = tid ->
                { model with
                    moving = Some (origin,cards,Point (oldX + x, oldY + y),s,id)
                    }
                    ,[]
            | _ -> returnModel model

        | CancelMove tid ->
            let returnCardsToOrigin =
                match model.moving with
                | Some (Pile pile,cards,_,_,id) when id = tid -> modifyPile pile ((@) cards)
                | Some (Tableau tableau,cards,_,_,id) when id = tid -> modifyTableauFaceUp tableau ((@) cards)
                | _ -> idFunc

            returnModel { returnCardsToOrigin model with moving = None}

        | CommitMove (target,tid) ->
            match model.moving with
            | Some (_,cards,_,_,id) when tid = id ->

                match cards,target with
                | _,Pile Stock | _,Pile Talon -> model, [ Msg (CancelMove id)]

                | [card],Pile pile when canPlaceOnFoundation (getPile pile model) (getSuit pile) card ->
                    let face = (snd card)
                    { pushCardToPile pile card model with moving = None },
                        [ 
                            Cmd (PlaySound ((getFaceContent face),(if face = KeySignature then NoOverlap else SoundMode.Overlap),1.0))
                            Msg MoveCommitted
                        ]
                        
                | cards,Tableau tableau when canPlaceOnTableau (getTableau tableau model |> faceUp) cards ->
                    { modifyTableauFaceUp tableau ((@) cards) model with moving = None }, [Msg MoveCommitted]

                | _ -> model,[Msg (CancelMove id)]
                
            | _ -> returnModel model

        | MoveCommitted ->
            if [model.heartsFoundation;model.spadesFoundation;model.diamondsFoundation;model.clubsFoundation] |> List.map List.length = [12;12;12;12]
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

        | CardTapped (_,face) -> model,[Cmd (PlaySound (getFaceContent face, (if face = KeySignature then NoOverlap else SoundMode.Overlap), 1.0))]

        | FlipTalon -> 
            model
            |> setPile Talon []
            |> setPile Stock (List.rev model.talon)
            |> returnModel

        | HandleMovingSound ->
            match model.moving with
            | Some (_,_,_,Some sound,_) -> 
                model,
                    [
                    Cmd (Delay (1.0,wrapOperation processMessage (Msg HandleMovingSound)))
                    Cmd (PlaySound (sound,SoundMode.Overlap,0.5))
                    ]
            | _ -> returnModel model

let update (gameState : GameState<Model, Tag>) : UpdateResult<Model, Tag> =
    let gestureResults =
        touchEvents gameState.model.previousTouches gameState.touches gameState.gameTime
        |> processEvents gameState.model.pendingGestures

    let model = 
        { gameState.model with
            pendingGestures = pendingGestures gestureResults
            previousTouches = gameState.touches
            }

    messages (List.rev gameState.objects) (gestures gestureResults)
    |> sendMessages processMessage
    |> evalState (UpdateResult (returnModel model))
