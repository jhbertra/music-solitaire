module Update

open Core
open Model

//
// --------- Msg ---------
//

type Msg =
    | DealCards
    | PopStock
    | BeginMove of Pile * int * (float * float)
    | Move of float * float
    | CancelMove
    | CommitMove of Pile
    | MoveCommitted
    | Reset



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
    printfn "%A" msg
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

    // Playing

    | PlayingPhase,PopStock -> 
        match model.stock with
        | [] -> model,Term
        | head::tail -> 
            { model with
                stock = tail
                talon = head :: model.talon
            }
            ,Term

    | PlayingPhase,(BeginMove (origin, count, pos)) ->
        match model.moving,origin,count with
        | None,Talon,1 when List.length model.talon > 0 ->
            { model with
                talon = List.skip 1 model.talon
                moving = Some (origin,(List.take 1 model.talon),pos)
                }
                ,Term
        | None,HeartsFoundation,1 when List.length model.heartsFoundation > 0 ->
            { model with
                heartsFoundation = List.skip 1 model.heartsFoundation
                moving = Some (origin,(List.take 1 model.heartsFoundation),pos)
                }
                ,Term
        | None,SpadesFoundation,1 when List.length model.spadesFoundation > 0 ->
            { model with
                spadesFoundation = List.skip 1 model.spadesFoundation
                moving = Some (origin,(List.take 1 model.spadesFoundation),pos)
                }
                ,Term
        | None,DiamondsFoundation,1 when List.length model.diamondsFoundation > 0 ->
            { model with
                diamondsFoundation = List.skip 1 model.diamondsFoundation
                moving = Some (origin,(List.take 1 model.diamondsFoundation),pos)
                }
                ,Term
        | None,ClubsFoundation,1 when List.length model.clubsFoundation > 0 ->
            { model with
                clubsFoundation = List.skip 1 model.clubsFoundation
                moving = Some (origin,(List.take 1 model.clubsFoundation),pos)
                }
                ,Term
        | None,Tableau1,n when n <= List.length model.tableau1 && n > 0 ->
            { model with
                tableau1 = List.skip n model.tableau1
                moving = Some (origin,(List.take n model.tableau1),pos)
                }
                ,Term
        | None,Tableau2,n when n <= List.length (fst model.tableau2) && n > 0 ->
            { model with
                tableau2 = (List.skip n (fst model.tableau2)),(snd model.tableau2)
                moving = Some (origin,(List.take n (fst model.tableau2)),pos)
                }
                ,Term
        | None,Tableau3,n when n <= List.length (fst model.tableau3) && n > 0 ->
            { model with
                tableau3 = (List.skip n (fst model.tableau3)),(snd model.tableau3)
                moving = Some (origin,(List.take n (fst model.tableau3)),pos)
                }
                ,Term
        | None,Tableau4,n when n <= List.length (fst model.tableau4) && n > 0 ->
            { model with
                tableau4 = (List.skip n (fst model.tableau4)),(snd model.tableau4)
                moving = Some (origin,(List.take n (fst model.tableau4)),pos)
                }
                ,Term
        | None,Tableau5,n when n <= List.length (fst model.tableau5) && n > 0 ->
            { model with
                tableau5 = (List.skip n (fst model.tableau5)),(snd model.tableau5)
                moving = Some (origin,(List.take n (fst model.tableau5)),pos)
                }
                ,Term
        | None,Tableau6,n when n <= List.length (fst model.tableau6) && n > 0 ->
            { model with
                tableau6 = (List.skip n (fst model.tableau6)),(snd model.tableau6)
                moving = Some (origin,(List.take n (fst model.tableau6)),pos)
                }
                ,Term
        | None,Tableau7,n when n <= List.length (fst model.tableau7) && n > 0 ->
            { model with
                tableau7 = (List.skip n (fst model.tableau7)),(snd model.tableau7)
                moving = Some (origin,(List.take n (fst model.tableau7)),pos)
                }
                ,Term
        | _ -> model,Term

    | PlayingPhase,(Move (x,y)) ->
        match model.moving with
        | None -> model,Term
        | Some (pile,cards,(oldX, oldY)) ->
            { model with
                moving = Some (pile,cards,(oldX + x, oldY + y))
                }
                ,Term
    | PlayingPhase,CancelMove ->
        match model.moving with
        | Some (Talon,cards,_) -> 
            { model with
                talon = cards @ model.talon
                moving = None
                }
                ,Term
        | Some (HeartsFoundation,cards,_) -> 
            { model with
                heartsFoundation = cards @ model.heartsFoundation
                moving = None
                }
                ,Term
        | Some (SpadesFoundation,cards,_) -> 
            { model with
                spadesFoundation = cards @ model.spadesFoundation
                moving = None
                }
                ,Term
        | Some (DiamondsFoundation,cards,_) -> 
            { model with
                diamondsFoundation = cards @ model.diamondsFoundation
                moving = None
                }
                ,Term
        | Some (ClubsFoundation,cards,_) -> 
            { model with
                clubsFoundation = cards @ model.clubsFoundation
                moving = None
                }
                ,Term
        | Some (Tableau1,cards,_) -> 
            { model with
                tableau1 = cards @ model.tableau1
                moving = None
                }
                ,Term
        | Some (Tableau2,cards,_) -> 
            { model with
                tableau2 = (cards @ (fst model.tableau2),(snd model.tableau2))
                moving = None
                }
                ,Term
        | Some (Tableau3,cards,_) -> 
            { model with
                tableau3 = (cards @ (fst model.tableau3),(snd model.tableau3))
                moving = None
                }
                ,Term
        | Some (Tableau4,cards,_) -> 
            { model with
                tableau4 = (cards @ (fst model.tableau4),(snd model.tableau4))
                moving = None
                }
                ,Term
        | Some (Tableau5,cards,_) -> 
            { model with
                tableau5 = (cards @ (fst model.tableau5),(snd model.tableau5))
                moving = None
                }
                ,Term
        | Some (Tableau6,cards,_) -> 
            { model with
                tableau6 = (cards @ (fst model.tableau6),(snd model.tableau6))
                moving = None
                }
                ,Term
        | Some (Tableau7,cards,_) -> 
            { model with
                tableau7 = (cards @ (fst model.tableau7),(snd model.tableau7))
                moving = None
                }
                ,Term
        | _ -> model,Term

    | PlayingPhase,(CommitMove target) ->
        match model.moving with
        | None -> model,Term
        | Some (_,cards,_) ->
            match cards,target with
            | [card],HeartsFoundation when canPlaceOnFoundation model.heartsFoundation Hearts card ->
                { model with
                    heartsFoundation = card :: model.heartsFoundation
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | [card],SpadesFoundation when canPlaceOnFoundation model.spadesFoundation Spades card ->
                { model with
                    spadesFoundation = card :: model.spadesFoundation
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | [card],DiamondsFoundation when canPlaceOnFoundation model.diamondsFoundation Diamonds card ->
                { model with
                    diamondsFoundation = card :: model.diamondsFoundation
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | [card],ClubsFoundation when canPlaceOnFoundation model.clubsFoundation Clubs card ->
                { model with
                    clubsFoundation = card :: model.clubsFoundation
                    moving = None
                    }
                    ,(Msg MoveCommitted)
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
            | _ -> model,(Msg CancelMove)

    | PlayingPhase,MoveCommitted ->
        if [model.heartsFoundation;model.spadesFoundation;model.diamondsFoundation;model.clubsFoundation] |> List.map List.length = [12;12;12;12]
        then
            { model with
                phase = WonPhase
                }
                ,Term
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

    | _,Reset -> initModel model.rng

    | _ -> model,Term