module Engine

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators

open FsGame
open FsGame.Core

open Model
open Draw

let optionToSingletonList = function
| Some a -> [a]
| None -> []

let returnModel = UseCases.returnModel

let evalState initial (State f) = f initial |> fst

let modify<'a> (f : 'a -> 'a) : State<'a,unit> = get >>= (put << f)

let processMessage msg model gameTime wrapOperation =
    if List.exists (function Step | Move _ -> false | _ -> true) [msg] then
        printfn "%A" msg
    else
        ()
    if model.won && msg <> Reset then
        returnModel model
    else
        match msg with

        | Step -> UseCases.step gameTime model

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

        | PlayMoveSound nextMsgs -> UseCases.playMoveSound wrapOperation (nextMsgs |> List.map Msg) model


let rec messagesFromGesture objects (id,gestureType) =
    match objects with
    | [] -> []
    | {tag = tag; box = box}::os ->
        let handler,hitPos =
            match gestureType with
            | Touch.GestureType.Tap pos -> tag.tapHandler <*> Some id, pos
            | Touch.GestureType.TouchDown pos -> tag.touchDownHandler <*> Some id, pos
            | Touch.GestureType.TouchUp pos -> tag.touchUpHandler <*> Some id, pos
            | Touch.GestureType.Drag (startPos,delta) -> tag.dragHandler <*> Some id <*> Some delta, startPos
        
        let result = handler <*> pointInBox hitPos box

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
    |> map (fun overlap -> Overlap (tag2,overlap))
    |> optionToSingletonList)
    @ overlaps id box os
| _ -> []

let rec messagesFromObjects = function
| [] -> []
| { tag = { id = tagId; overlapHandler = Some handler }; box = box } :: os ->
    overlaps tagId box os
    |> List.map handler
    |> List.choose id
    |> List.append (messagesFromObjects os)
| _ :: os -> messagesFromObjects os

let messages objects gestures gameTime = Step :: (gestures |> List.collect (messagesFromGesture objects)) @ messagesFromObjects objects

let setModel model (UpdateResult (_,cmds)) = UpdateResult (model,cmds)

let addCommands commands (UpdateResult (model,cmds)) = UpdateResult (model,cmds @ commands)

let getModel = 
    monad { 
        let! UpdateResult (model,_) = get
        return model 
    }

let rec sendMessages gameTime = function
| [] -> get
| msg::msgs ->

    let rec messages = function
    | [] -> []
    | (Cmd _) :: ops -> messages ops
    | (Msg msg) :: ops -> msg :: messages ops

    let rec commands = function
    | [] -> []
    | (Msg _) :: ops -> commands ops
    | (Cmd cmd) :: ops -> cmd :: commands ops

    monad {
        let! model = getModel
        let model,nextOperations = processMessage msg model gameTime wrapOperation
        do! modify (setModel model)
        do! commands nextOperations |> addCommands |> modify
        return! sendMessages gameTime ((messages nextOperations) @ msgs)
    }


and wrapOperation = function
| Msg msg -> fun {model=model; gameTime = gameTime} -> sendMessages gameTime [msg] |> evalState (UpdateResult (returnModel model))
| Cmd cmd -> fun {model=model} -> UpdateResult (model, [cmd])


let update (gameState : GameState<Model, Touch.Touch list, Tag>) : UpdateResult<Model, Touch.Touch list, Tag> =

    let gestureResults =
        Touch.touchEvents gameState.model.previousTouches gameState.controller gameState.gameTime
        |> Touch.processEvents gameState.model.pendingGestures

    let model = 
        { gameState.model with
            pendingGestures = Touch.pendingGestures gestureResults
            previousTouches = gameState.controller
            }

    messages (List.rev gameState.objects) (Touch.gestures gestureResults) gameState.gameTime 
    |> sendMessages gameState.gameTime
    |> evalState (UpdateResult (returnModel model))


let engine (rng : System.Random) = {
    contentManifest = contentManifest
    init = UseCases.initialize rng
    update = update
    draw = draw
}
