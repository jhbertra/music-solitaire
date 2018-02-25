module Engine

open Core
open FsGame.Platform
open Touch
open Model
open Draw

let returnModel = UseCases.returnModel

let processMessage msg model gameTime wrapOperation =
    if model.won && msg <> Reset then
        returnModel model
    else
        match msg with

        | Step -> UseCases.advanceTime model gameTime wrapOperation

        | Reset -> UseCases.initialize model.rng |> returnModel

        | PreparePop -> returnModel { model with popReady = true }

        | PopStock -> UseCases.popStock model |> returnModel

        | BeginMove (origin, count, pos, id) -> UseCases.pickUpCards origin count pos id model |> returnModel

        | Move ( id , delta ) -> UseCases.moveHand id delta model |> returnModel

        | CancelMove -> UseCases.cancelMove model |> returnModel

        | StageMove ( target, point ) -> UseCases.stageMove target point gameTime model |> returnModel

        | UnstageMove -> UseCases.unstageMove model |> returnModel

        | CommitMove id -> UseCases.commitMove id model

        | MoveCommitted -> UseCases.checkForWin wrapOperation model

        | CardTapped ( Card ( _ , face ) ) -> 
            ( model
            , [Cmd (PlaySound (getFaceContent face, (if face = KeySignature then NoOverlap else SoundMode.Overlap), 1.0))]
            )

        | FlipTalon -> UseCases.recycleTalon model |> returnModel


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

let rec sendMessages gameTime = function
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
        let model,nextOperations = processMessage msg model gameTime wrapOperation
        do! State.modify (setModel model)
        for cmd in commands nextOperations do
            do! State.modify (addCommand cmd)
        return! sendMessages gameTime ((messages nextOperations) @ msgs)
    }


and wrapOperation = function
| Msg msg -> fun {model=model; gameTime = gameTime} -> sendMessages gameTime [msg] |> State.eval (UpdateResult (returnModel model))
| Cmd cmd -> fun {model=model} -> UpdateResult (model, [cmd])


let update (gameState : GameState<Model, Tag>) : UpdateResult<Model, Tag> =

    let gestureResults =
        touchEvents gameState.model.previousTouches gameState.touches gameState.gameTime
        |> processEvents gameState.model.pendingGestures

    let model = 
        { gameState.model with
            pendingGestures = pendingGestures gestureResults
            previousTouches = gameState.touches
            }

    messages (List.rev gameState.objects) (gestures gestureResults) gameState.gameTime 
    |> sendMessages gameState.gameTime
    |> State.eval (UpdateResult (returnModel model))


let engine (rng : System.Random) = {
    contentManifest = contentManifest
    init = UseCases.initialize rng
    update = update
    draw = draw
}
