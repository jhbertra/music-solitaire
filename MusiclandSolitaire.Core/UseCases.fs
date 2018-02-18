﻿module UseCases

open Core
open FsGame.Platform
open Touch
open Model

//
// --------- Util ---------
//

let returnModel model = ( model , [] )



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

    State.eval dealingState (deal deck)



//
// --------- Advance Time For Animations ---------
//

let advanceTime model gameTime =
    let move gameTime progress = 
        let speed = (10.0 - 30.0) * progress + 30.0
        let step = ((float gameTime.elapsed.Milliseconds) * 0.001) * speed
        min 1.0 (progress + step)

    match model.hand with
    | Some ( o , c , p , i , Some ( Unstaging ( origin , progress ) ) ) ->
        let progress = move gameTime progress
        let staging = 
            if progress = 1.0 then 
                  None 
              else 
                  Some ( Unstaging ( origin, progress ) )

        { model with hand = Some ( o , c , p , i , staging ) }

    | Some ( o , c , p , i , Some ( Staged ( target, origin, dest, progress ) ) ) ->
        let progress = move gameTime progress

        { model with hand = Some ( o , c , p , i , Some ( Staged ( target, origin, dest, progress ) ) ) }

    | _ -> model



//
// --------- Move a Card from Stock to Talon ---------
//

let popStock model =
    match (model.popReady),(model.stock) with
    | false,_ -> model

    | true,[] -> model

    | true,(head::tail) -> 
        { model with
            stock = tail
            talon = head :: model.talon
            popReady = false
        }



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

let pickUpCards origin count pos id model =
    match ( model.hand , origin , count ) with
    | None , MoveOrigin.Talon , 1 ->
        { modifyTalon (List.skip 1) model with
            hand = Some ( origin , List.take 1 model.talon , pos , id , None )
            }

    | None , MoveOrigin.Foundation suit , 1 ->
        { modifyFoundation suit (List.skip 1) model with
            hand = Some ( origin , getFoundation suit model |> cardsInFoundation |> List.take 1 , pos , id , None )
            }

    | None, MoveOrigin.Tableau tableau, n when n <= (getTableau tableau model |> faceUp |> List.length) && n > 0 ->
        { modifyTableauFaceUp tableau (List.skip n) model with
            hand = Some ( origin , getTableau tableau model |> faceUp |> List.take n , pos , id , None )
            }

    | _ -> model



//
// --------- Move the Hand ---------
//

let moveHand touchId ( Delta ( x , y ) ) model =
    match model.hand with
    | Some ( origin , cards , Point ( oldX , oldY ) , id , staging ) when id = touchId ->
        { model with
            hand = Some ( origin , cards , Point ( oldX + x , oldY + y ) , id , staging )
            }

    | _ -> model



//
// --------- Cancel a Move ---------
//

let cancelMove model =
    let hand = model.hand
    let model = { model with hand = None }

    match hand with
    | Some ( MoveOrigin.Talon , cards , _ , id , _ ) -> 
        modifyTalon ( (@) cards ) model

    | Some ( MoveOrigin.Foundation foundation , cards , _ , id , _ ) -> 
        modifyFoundation foundation ( (@) cards ) model

    | Some ( MoveOrigin.Tableau tableau , cards , _ , id , _ ) -> 
        modifyTableauFaceUp tableau ( (@) cards ) model

    | None -> model



//
// --------- Stage a Move ---------
//

let stageMove face target point wrapOperation model =
    match model.hand with
    | Some ( origin , cards , movingPos , id , None ) -> 
        let movingFace = List.last cards |> Model.face

        ( { model with hand = Some ( origin , cards , movingPos , id , Some ( Staged ( target, movingPos, point, 0.0 ) ) ) }
        , [ PlaySound ( getFaceContent face, (if face = KeySignature then NoOverlap else SoundMode.Overlap), 1.0 )
            Delay ( 0.33, PlaySound ( getFaceContent movingFace, (if movingFace = KeySignature then NoOverlap else SoundMode.Overlap), 1.0 ) |> Cmd |> wrapOperation )
          ] 
          |> List.map Cmd
        )

    | _ -> returnModel model



//
// --------- Unstage a Move ---------
//

let unstageMove target model =
    match model.hand with
    | Some ( o , c , p , i , Some ( Staged (  moveTarget, origin, dest, progress ) ) ) when target = moveTarget -> 
        { model with hand = Some ( o , c , p , i , Some ( Unstaging ( lerp origin dest progress, 0.0 ) ) ) }

    | _ -> model



//
// --------- Commit a Staged Move ---------
//

let commitMove touchId model =
    match model.hand with
    | Some ( _ , cards , _ , id , Some ( Staged ( target, _, _, _ ) ) ) when touchId = id ->

        match ( cards , target ) with

        | [card] , MoveTarget.Foundation foundation when canPlaceOnFoundation (getFoundation foundation model) card ->
            let face = face card
            ( pushCardToFoundation foundation card { model with hand = None }
            , [ 
                  Cmd (PlaySound ((getFaceContent face),(if face = KeySignature then NoOverlap else SoundMode.Overlap),1.0))
                  Msg MoveCommitted
              ]
            )
                
        | cards , MoveTarget.Tableau tableau when canPlaceOnTableau (getTableau tableau model |> faceUp) cards ->
            ( modifyTableauFaceUp tableau ((@) cards) { model with hand = None } , [Msg MoveCommitted] )

        | _ -> ( model , [Msg CancelMove] )
    
    | Some ( _ , _ , _ , id , None ) -> ( model , [Msg CancelMove] )

    | _ -> returnModel model



//
// --------- Check for a Win ---------
//

let checkForWin wrapOperation model =
    let foundations = suits |> List.map ((flip getFoundation) model) |> List.map cardsInFoundation

    if foundations |> List.map List.length = [12;12;12;12] then
        ( { model with won = true }
        , [Cmd ( Delay ( 1.0 , wrapOperation ( Cmd ( PlaySound ( "Stack" , NoOverlap , 1.0 ) ) ) ) )]
        )
    else 
        model
        |> modifyTableau Tableau2 replenish
        |> modifyTableau Tableau3 replenish
        |> modifyTableau Tableau4 replenish
        |> modifyTableau Tableau5 replenish
        |> modifyTableau Tableau6 replenish
        |> modifyTableau Tableau7 replenish
        |> returnModel