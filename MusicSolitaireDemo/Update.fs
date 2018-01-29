module Update

open Core
open Model

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

type Msg =
    | DealCards
    | PreparePop
    | PopStock
    | BeginMove of Pile * int * (float * float)*int
    | Move of (float * float)*int
    | TouchDropped of int
    | CommitMove of Pile*int
    | MoveCommitted
    | Reset
    | CardTapped of Card
    | FlipStock
    | HandleMovingSound



//
// --------- Init ---------
//

let initModel rng = 
    {
    stock = []
    talon = []
    tableau1 = []
    tableau2 = [],[]
    tableau3 = [],[]
    tableau4 = [],[]
    tableau5 = [],[]
    tableau6 = [],[]
    tableau7 = [],[]
    heartsFoundation = []
    spadesFoundation = []
    diamondsFoundation = []
    clubsFoundation = []
    moving = None
    rng = rng
    popReady = false
    phase = DealPhase 
        {
        deck = 
            [for suit in suits do for face in faces do yield suit,face]
            |> Array.ofList
            |> shuffleArr rng
            |> List.ofArray
        tableauDealMoves = 
            let numberOrder = [1..7]
            numberOrder |> List.collect (fun i -> (List.skip (i-1) numberOrder) |> List.mapi (fun j num -> num,j=0))
        }
    }
    ,(Msg DealCards)



//
// --------- Update ---------
//

let updateTableau tableau faceUp card =
    match tableau with up,down -> if faceUp then card::up,down else up,card::down

let isFace2Higher face1 face2 =
    face1 = KeySignature && face2 = Do
    || face1 = Do && face2 = Re
    || face1 = Re && face2 = Mi
    || face1 = Mi && face2 = Fa
    || face1 = Fa && face2 = So
    || face1 = So && face2 = La
    || face1 = La && face2 = Ti
    || face1 = Ti && face2 = Do8
    || face1 = Do8 && face2 = IV
    || face1 = IV && face2 = V
    || face1 = V && face2 = I

let areAlternateSuits suit1 suit2 =
    match suit1 with
    | Hearts | Diamonds -> suit2 = Clubs || suit2 = Spades
    | _ -> suit2 = Hearts || suit2 = Diamonds

let canPlaceOnFoundation foundation requiredSuit card =
    match foundation,card with
    | [],(suit,KeySignature) -> suit = requiredSuit
    | (_,targetFace)::_,(suit,face) -> isFace2Higher targetFace face && suit = requiredSuit
    | _ -> false

let canPlaceOnTableau tableau cards =
    match tableau,(List.rev cards) with
    | [],((_,I) :: _) -> true
    | ((targetSuit, targetFace) :: _),((suit,face) :: _) -> 
        isFace2Higher face targetFace && areAlternateSuits targetSuit suit 
    | _ -> false

let replenish destAndSource =
    match destAndSource with
    | [],(head :: tail) -> [head],tail
    | _ -> destAndSource

let update msg model =
    match model.phase,msg with

    // Dealing

    | (DealPhase dealModel),DealCards -> 
        match dealModel.deck with
        | [] -> { model with phase = PlayingPhase },Term
        | card :: remainingDeck -> 
            match dealModel.tableauDealMoves with
            | move :: remainingMoves -> 
                match move with
                | 1,_ -> { model with tableau1 = [card]; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | 2,faceUp -> { model with tableau2 = updateTableau model.tableau2 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | 3,faceUp -> { model with tableau3 = updateTableau model.tableau3 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | 4,faceUp -> { model with tableau4 = updateTableau model.tableau4 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | 5,faceUp -> { model with tableau5 = updateTableau model.tableau5 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | 6,faceUp -> { model with tableau6 = updateTableau model.tableau6 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | 7,faceUp -> { model with tableau7 = updateTableau model.tableau7 faceUp card; phase = DealPhase { deck = remainingDeck; tableauDealMoves = remainingMoves } }
                | _ -> model
            | [] -> { model with stock = card :: model.stock; phase = DealPhase { deck = remainingDeck; tableauDealMoves = [] } }
            ,Msg DealCards

    | (DealPhase _),_ -> model,Term        

    // Playing

    | PlayingPhase,PreparePop -> 
        { model with
            popReady = true
            }
            ,Term

    | PlayingPhase,PopStock -> 
        match (model.popReady),(model.stock) with
        | false,_ -> model,Term
        | true,[] -> model,Term
        | true,(head::tail) -> 
            { model with
                stock = tail
                talon = head :: model.talon
                popReady = false
            }
            ,Term

    | PlayingPhase,(BeginMove (origin, count, pos, id)) ->
        let pileSound = List.head >> snd >> getFaceContent >> Some
        let tableauSound = (fun n -> fst >> List.take n >> List.last >> snd >> getFaceContent >> Some)
        let susSound = Option.map (fun s -> s + "_Sus")
        let newModel,sound =
            match model.moving,origin,count with
            | None,Talon,1 when not (List.isEmpty model.talon) ->
                let sound = pileSound model.talon
                { model with
                    talon = List.skip 1 model.talon
                    moving = Some (origin,(List.take 1 model.talon),pos,susSound sound, id)
                    }
                    ,sound
            | None,HeartsFoundation,1 when not (List.isEmpty model.heartsFoundation) ->
                let sound = pileSound model.heartsFoundation
                { model with
                    heartsFoundation = List.skip 1 model.heartsFoundation
                    moving = Some (origin,(List.take 1 model.heartsFoundation),pos,susSound sound, id)
                    }
                    ,sound
            | None,SpadesFoundation,1 when not (List.isEmpty model.spadesFoundation) ->
                let sound = pileSound model.spadesFoundation
                { model with
                    spadesFoundation = List.skip 1 model.spadesFoundation
                    moving = Some (origin,(List.take 1 model.spadesFoundation),pos,susSound sound, id)
                    }
                    ,sound
            | None,DiamondsFoundation,1 when not (List.isEmpty model.diamondsFoundation) ->
                let sound = pileSound model.diamondsFoundation
                { model with
                    diamondsFoundation = List.skip 1 model.diamondsFoundation
                    moving = Some (origin,(List.take 1 model.diamondsFoundation),pos,susSound sound, id)
                    }
                    ,sound
            | None,ClubsFoundation,1 when not (List.isEmpty model.clubsFoundation) ->
                let sound = pileSound model.clubsFoundation
                { model with
                    clubsFoundation = List.skip 1 model.clubsFoundation
                    moving = Some (origin,(List.take 1 model.clubsFoundation),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau1,n when n <= List.length model.tableau1 && n > 0 ->
                let sound = tableauSound n (model.tableau1,[])
                { model with
                    tableau1 = List.skip n model.tableau1
                    moving = Some (origin,(List.take n model.tableau1),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau2,n when n <= List.length (fst model.tableau2) && n > 0 ->
                let sound = tableauSound n model.tableau2
                { model with
                    tableau2 = (List.skip n (fst model.tableau2)),(snd model.tableau2)
                    moving = Some (origin,(List.take n (fst model.tableau2)),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau3,n when n <= List.length (fst model.tableau3) && n > 0 ->
                let sound = tableauSound n model.tableau3
                { model with
                    tableau3 = (List.skip n (fst model.tableau3)),(snd model.tableau3)
                    moving = Some (origin,(List.take n (fst model.tableau3)),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau4,n when n <= List.length (fst model.tableau4) && n > 0 ->
                let sound = tableauSound n model.tableau4
                { model with
                    tableau4 = (List.skip n (fst model.tableau4)),(snd model.tableau4)
                    moving = Some (origin,(List.take n (fst model.tableau4)),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau5,n when n <= List.length (fst model.tableau5) && n > 0 ->
                let sound = tableauSound n model.tableau5
                { model with
                    tableau5 = (List.skip n (fst model.tableau5)),(snd model.tableau5)
                    moving = Some (origin,(List.take n (fst model.tableau5)),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau6,n when n <= List.length (fst model.tableau6) && n > 0 ->
                let sound = tableauSound n model.tableau6
                { model with
                    tableau6 = (List.skip n (fst model.tableau6)),(snd model.tableau6)
                    moving = Some (origin,(List.take n (fst model.tableau6)),pos,susSound sound, id)
                    }
                    ,sound
            | None,Tableau7,n when n <= List.length (fst model.tableau7) && n > 0 ->
                let sound = tableauSound n model.tableau7
                { model with
                    tableau7 = (List.skip n (fst model.tableau7)),(snd model.tableau7)
                    moving = Some (origin,(List.take n (fst model.tableau7)),pos,susSound sound, id)
                    }
                    ,sound
            | _ -> model,None
        match newModel,sound with
        | model,None -> model,Term
        | model,(Some sound) -> 
            model,
            Delay (
                1.0,
                PlaySound (
                    sound,
                    NoOverlap,
                    1.0,
                    PlaySound (sound + "_Sus",Overlap,0.5,Term)
                ),
                Msg HandleMovingSound
            )

    | PlayingPhase,(Move ((x,y),tid)) ->
        match model.moving with
        | Some (pile,cards,(oldX, oldY),s,id) when id = tid ->
            { model with
                moving = Some (pile,cards,(oldX + x, oldY + y),s,id)
                }
                ,Term
        | _ -> model,Term
    | PlayingPhase,TouchDropped tid ->
        match model.moving with
        | Some (Talon,cards,_,_,id) when id = tid -> 
            { model with
                talon = cards @ model.talon
                moving = None
                }
                ,Term
        | Some (HeartsFoundation,cards,_,_,id) when id = tid -> 
            { model with
                heartsFoundation = cards @ model.heartsFoundation
                moving = None
                }
                ,Term
        | Some (SpadesFoundation,cards,_,_,id) when id = tid -> 
            { model with
                spadesFoundation = cards @ model.spadesFoundation
                moving = None
                }
                ,Term
        | Some (DiamondsFoundation,cards,_,_,id) when id = tid -> 
            { model with
                diamondsFoundation = cards @ model.diamondsFoundation
                moving = None
                }
                ,Term
        | Some (ClubsFoundation,cards,_,_,id) when id = tid -> 
            { model with
                clubsFoundation = cards @ model.clubsFoundation
                moving = None
                }
                ,Term
        | Some (Tableau1,cards,_,_,id) when id = tid -> 
            { model with
                tableau1 = cards @ model.tableau1
                moving = None
                }
                ,Term
        | Some (Tableau2,cards,_,_,id) when id = tid -> 
            { model with
                tableau2 = (cards @ (fst model.tableau2),(snd model.tableau2))
                moving = None
                }
                ,Term
        | Some (Tableau3,cards,_,_,id) when id = tid -> 
            { model with
                tableau3 = (cards @ (fst model.tableau3),(snd model.tableau3))
                moving = None
                }
                ,Term
        | Some (Tableau4,cards,_,_,id) when id = tid -> 
            { model with
                tableau4 = (cards @ (fst model.tableau4),(snd model.tableau4))
                moving = None
                }
                ,Term
        | Some (Tableau5,cards,_,_,id) when id = tid -> 
            { model with
                tableau5 = (cards @ (fst model.tableau5),(snd model.tableau5))
                moving = None
                }
                ,Term
        | Some (Tableau6,cards,_,_,id) when id = tid -> 
            { model with
                tableau6 = (cards @ (fst model.tableau6),(snd model.tableau6))
                moving = None
                }
                ,Term
        | Some (Tableau7,cards,_,_,id) when id = tid -> 
            { model with
                tableau7 = (cards @ (fst model.tableau7),(snd model.tableau7))
                moving = None
                }
                ,Term
        | _ -> model,Term

    | PlayingPhase,(CommitMove (target,tid)) ->
        match model.moving with
        | Some (_,cards,_,_,id) when tid = id ->
            match cards,target with
            | [card],HeartsFoundation when canPlaceOnFoundation model.heartsFoundation Hearts card ->
                { model with
                    heartsFoundation = card :: model.heartsFoundation
                    moving = None
                    }
                    ,(PlaySound ((getFaceContent (snd card)),NoOverlap,1.0,(Msg MoveCommitted)))
            | [card],SpadesFoundation when canPlaceOnFoundation model.spadesFoundation Spades card ->
                { model with
                    spadesFoundation = card :: model.spadesFoundation
                    moving = None
                    }
                    ,(PlaySound ((getFaceContent (snd card)),NoOverlap,1.0,(Msg MoveCommitted)))
            | [card],DiamondsFoundation when canPlaceOnFoundation model.diamondsFoundation Diamonds card ->
                { model with
                    diamondsFoundation = card :: model.diamondsFoundation
                    moving = None
                    }
                    ,(PlaySound ((getFaceContent (snd card)),NoOverlap,1.0,(Msg MoveCommitted)))
            | [card],ClubsFoundation when canPlaceOnFoundation model.clubsFoundation Clubs card ->
                { model with
                    clubsFoundation = card :: model.clubsFoundation
                    moving = None
                    }
                    ,(PlaySound ((getFaceContent (snd card)),NoOverlap,1.0,(Msg MoveCommitted)))
            | cards,Tableau1 when canPlaceOnTableau model.tableau1 cards ->
                { model with
                    tableau1 = cards @ model.tableau1
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | cards,Tableau2 when canPlaceOnTableau (fst model.tableau2) cards ->
                { model with
                    tableau2 = (cards @ (fst model.tableau2)),(snd model.tableau2)
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | cards,Tableau3 when canPlaceOnTableau (fst model.tableau3) cards ->
                { model with
                    tableau3 = (cards @ (fst model.tableau3)),(snd model.tableau3)
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | cards,Tableau4 when canPlaceOnTableau (fst model.tableau4) cards ->
                { model with
                    tableau4 = (cards @ (fst model.tableau4)),(snd model.tableau4)
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | cards,Tableau5 when canPlaceOnTableau (fst model.tableau5) cards ->
                { model with
                    tableau5 = (cards @ (fst model.tableau5)),(snd model.tableau5)
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | cards,Tableau6 when canPlaceOnTableau (fst model.tableau6) cards ->
                { model with
                    tableau6 = (cards @ (fst model.tableau6)),(snd model.tableau6)
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | cards,Tableau7 when canPlaceOnTableau (fst model.tableau7) cards ->
                { model with
                    tableau7 = (cards @ (fst model.tableau7)),(snd model.tableau7)
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | _ -> model,(Msg (TouchDropped id))
        | _ -> model,Term

    | PlayingPhase,MoveCommitted ->
        if [model.heartsFoundation;model.spadesFoundation;model.diamondsFoundation;model.clubsFoundation] |> List.map List.length = [12;12;12;12]
        then
            { model with
                phase = WonPhase
                }
                ,(Delay (1.0,Term,(PlaySound ("Stack",NoOverlap,1.0,Term))))
        else 
            { model with
                tableau2 = replenish model.tableau2
                tableau3 = replenish model.tableau3
                tableau4 = replenish model.tableau4
                tableau5 = replenish model.tableau5
                tableau6 = replenish model.tableau6
                tableau7 = replenish model.tableau7
                }
                ,Term            

    | PlayingPhase,DealCards -> model,Term

    | PlayingPhase,(CardTapped (_,face)) -> model,PlaySound (getFaceContent face,NoOverlap,1.0,Term)

    | PlayingPhase,FlipStock -> 
        { model with
            talon = []
            stock = List.rev model.talon 
            }
            ,Term

    | PlayingPhase,HandleMovingSound ->
        match model.moving with
        | Some (_,_,_,Some sound,_) -> model,Delay (1.0,PlaySound (sound,Overlap,0.5,Term),Msg HandleMovingSound)
        | _ -> model,Term

    // Game End


    | _,Reset -> initModel model.rng

    | WonPhase,_ -> model,Term


//
// --------- Subscriptions ---------
//

let subscriptions model =
    if Option.isSome model.moving then
        [(Sub.TouchDropped TouchDropped)]
    else
        []