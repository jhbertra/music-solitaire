module UseCases

open Aether
open Aether.Operators
open FSharpPlus
open FSharpPlus.Operators

open FsGame.Core

open Model
open FSharpPlus.Data
open FSharpPlus.Data

//
// --------- Util ---------
//

let returnModel model = ( model , [] )


let shuffleArr (rng: System.Random) arr =
    let array = Array.copy arr
    let n = array.Length
    for x in 1..n do
        let i = n-x
        let j = rng.Next(i+1)
        let tmp = array.[i]
        array.[i] <- array.[j]
        array.[j] <- tmp
    array



//
// --------- Init ---------
//

let rec deal dealState = function
| [] -> dealState.model
| card :: cards ->
    let dealState =
        match dealState.tableauDealMoves with
        | [] -> { dealState with model = (dealState.model |> pushCardToStock card ) }

        | (tableau, faceUp) :: moves ->
            { dealState with
                tableauDealMoves = moves
                model = pushCardToTableau tableau faceUp card dealState.model
            }

    deal dealState cards


let initialize rng =

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
        hand = None
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

    deal dealingState deck



//
// --------- Logic to Evaluate Every Frame ---------
//

let step gameTime model =
    let move gameTime progress =
        let speed = (10.0 - 30.0) * progress + 30.0
        let step = ((float gameTime.elapsed.TotalMilliseconds) * 0.001) * speed
        min 1.0 (progress + step)

    match model.hand with
    | Some hand ->
        match hand^.handStaging with
        | Some (Unstaging unstaging) ->
            let progress = move gameTime (unstaging^.unstagingProgress)
            let staging =
                if progress = 1.0 then
                      None
                  else
                      Optic.set (unstagingProgress) progress unstaging |> Unstaging |> Some

            returnModel { model with hand = hand |> (staging ^= handStaging) |> Some }

        | Some (Staged s) when distance (hand^.handLocation) (s^.stagedDest) |> abs <= 56.0 ->
            let progress = move gameTime (s^.stagedProgress)

            let model = { model with hand = hand |> (progress ^= (handStaging >-> optionSome >?> staged >?> stagedProgress)) |> Some }

            if not (s^.stagedPlayedSound) && gameTime.total.TotalMilliseconds - (s^.stagedTimeStaged).total.TotalMilliseconds >= 500.0 then
                ( model, [ Msg ( PlayMoveSound [] ) ] )
            else
                returnModel model
        | Some _ ->
            ( model
            , [ Msg UnstageMove ]
            )

        | _ -> returnModel model

    | _ -> returnModel model





//
// --------- Play Move Sound ---------
//

let playMoveSound wrapOperation nextMsgs model =
    monad {
        let! hand = model.hand^.Option.value_
        let! staged = hand^.(handStaging >-> Option.value_ >?> staged)
        if not (staged^.stagedPlayedSound) then
            let targetFace = getTopCard (staged^.stagedTarget) model |> map (Optic.get cardFace)

            let targetSound =
                match targetFace with
                | None | Some KeySignature -> None
                | Some face -> getFaceContent face |> Some

            let movingSound = List.last (hand^.handCards) |> Optic.get cardFace |> getFaceContent

            let newHand = hand |> Optic.set handStaging (staged |> Optic.set stagedPlayedSound true |> Staged |> Some)

            return
                ( { model with hand = Some newHand }
                , nextMsgs
                  @ match ( targetSound ) with
                     | ( Some sound ) ->
                           [
                           Cmd ( PlaySound ( sound , SoundMode.Overlap , 1.0 ) )
                           Cmd
                             ( Delay
                                 ( 0.33
                                 , Cmd
                                     ( PlaySound
                                         ( movingSound
                                         , (if movingSound = getFaceContent KeySignature then SoundMode.NoOverlap else SoundMode.Overlap)
                                         , 1.0
                                         )
                                     )  |> wrapOperation
                                 )
                             )
                           ]

                     | None ->
                           [
                           Cmd
                             ( PlaySound
                                ( movingSound
                                , (if movingSound = getFaceContent KeySignature then SoundMode.NoOverlap else SoundMode.Overlap)
                                , 1.0
                                )
                             )
                           ]
                )
        else
            return returnModel model
    }
    |> Option.defaultValue ( model , nextMsgs )


//
// --------- Move a Card from Stock to Talon ---------
//

let popStock model =
    match (model.popReady),(model.stock) with
    | true,(head::tail) ->
        { model with
            stock = tail
            talon = head :: model.talon
            popReady = false
        }
    | _ -> model



//
// --------- Refill Empty Stock With Talon ---------
//

let recycleTalon model =
    match model.stock with
    | [] -> { model with talon = []; stock = List.rev model.talon }
    | _ -> model



//
// --------- Pick Up Cards From Talon, Foundation, or Tableau ---------
//

let pickUpCards origin count pos identifier model =
    monad {
        let! cardsLens =
            match (model.hand, origin, count) with
            | (None, MoveOrigin.Talon , 1) -> Some modelTalon
            | (None, MoveOrigin.Foundation s , 1) -> modelFoundation s >-> foundationCards |> Some
            | (None, MoveOrigin.Tableau t , _) -> modelTableau t >-> tableauFaceUp |> Some
            | _ -> None
        return
            { Optic.map cardsLens (List.skip count) model with
                hand = Some(origin, model^.cardsLens |> List.take count, pos, identifier, None) }
    }
    |> Option.defaultValue model


//
// --------- Move the Hand ---------
//

let moveHand touchId ( Delta ( x , y ) ) =
    Optic.map
        (modelHand >-> optionSome >?> handLocation)
        (fun (Point(oldX , oldY)) -> Point(oldX + x, oldY + y))



//
// --------- Stage a Move ---------
//

let stageMove target point gameTime =
    Optic.map
        (modelHand >-> optionSome)
        (fun hand ->
            Optic.map
                (handStaging)
                (Some << Option.defaultValue (StagedModel(target, hand^.handLocation, point, 0.0, gameTime, false) |> Staged))
                hand)



//
// --------- Unstage a Move ---------
//

let unstageMove =
    Optic.map
        (modelHand >-> optionSome >?> handStaging >?> optionSome)
        (function
        | Staged s ->
            let origin = s^.stagedOrigin
            let dest = s^.stagedDest
            let progress = s^.stagedProgress
            UnstagingModel (lerp origin dest progress, 0.0) |> Unstaging
        | s -> s)



//
// --------- Commit a Staged Move ---------
//

let commitMove touchId model =
    monad {
        let! hand = model^.modelHand
        let handId = hand^.handTouchId
        let! s = hand^.(handStaging >-> optionSome >?> staged) |> filter (touchId = handId |> konst)
        if s^.stagedPlayedSound then
            return
                match ( hand^.handCards , s^.stagedTarget ) with
                    | [card] , MoveTarget.Foundation f when canPlaceOnFoundation (model^.(modelFoundation f)) card ->
                        ( Optic.map (modelFoundation f) (pushCardToFoundation card) { model with hand = None }
                        , [Msg MoveCommitted]
                        )
                    | cards , MoveTarget.Tableau t when canPlaceOnTableau (model^.(modelTableau t >-> tableauFaceUp)) cards ->
                        ( Optic.map (modelTableau t >-> tableauFaceUp) ((@) cards) { model with hand = None }
                        , [Msg MoveCommitted]
                        )
                    | _ -> ( model , [Msg CancelMove] )
        else
            return (model, [Msg(PlayMoveSound[CommitMove touchId])]) }
    <|> monad {
        let! _ = model.hand |> filter (Optic.get handStaging >> Option.isNone)
        return ( model , [Msg CancelMove] ) }
    |> Option.defaultValue (returnModel model)



//
// --------- Cancel a Move ---------
//

let cancelMove =
    stateOption {
        let! hand = clearL modelHand
        let cardsLens =
            match hand^.handOrigin with
            | MoveOrigin.Talon -> modelTalon
            | MoveOrigin.Foundation s -> modelFoundation s >-> foundationCards
            | MoveOrigin.Tableau t -> modelTableau t >-> tableauFaceUp
        do! modifyState (Optic.map cardsLens ((@) (hand^.handCards))) |> liftState }
    |> State.exec



//
// --------- Check for a Win ---------
//

let checkForWin wrapOperation model =
    let foundations =
        suits
        |> List.map (modelFoundation >> (flip Compose.lens) foundationCards >> (flip Optic.get) model)

    if foundations |> List.map List.length = [12;12;12;12] then
        ( { model with won = true }
        , [Cmd ( Delay ( 1.0 , wrapOperation ( Cmd ( PlaySound ( "Stack" , NoOverlap , 1.0 ) ) ) ) )]
        )
    else
        model
        |> Optic.map (modelTableau Tableau2) replenish
        |> Optic.map (modelTableau Tableau3) replenish
        |> Optic.map (modelTableau Tableau4) replenish
        |> Optic.map (modelTableau Tableau5) replenish
        |> Optic.map (modelTableau Tableau6) replenish
        |> Optic.map (modelTableau Tableau7) replenish
        |> returnModel