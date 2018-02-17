module Model

open Core
open Touch
open FsGame

//
// --------- Types ---------
//

type Suit =
    | Hearts
    | Clubs
    | Diamonds
    | Spades


type Face =
    | KeySignature
    | Do
    | Re
    | Mi
    | Fa
    | So
    | La
    | Ti
    | Do8
    | IV
    | V
    | I


type Card = Card of Suit * Face


type TableauNumber =
    | Tableau1
    | Tableau2
    | Tableau3
    | Tableau4
    | Tableau5
    | Tableau6
    | Tableau7


type MoveOrigin =
    | Talon
    | Foundation of Suit
    | Tableau of TableauNumber


type MoveTarget =
    | Foundation of Suit
    | Tableau of TableauNumber


type Tableau = Tableau of Card list * Card list


type Foundation = Foundation of Card list * Suit


type MovingModel = MoveOrigin * Card list * Point * int


type MoveModel = MoveModel of MoveTarget * Point * Point * float


type UnstagingModel = UnstagingModel of Point * float


type Model = {
    won : bool
    stock : Card list
    talon : Card list
    tableau1 : Tableau
    tableau2 : Tableau
    tableau3 : Tableau
    tableau4 : Tableau
    tableau5 : Tableau
    tableau6 : Tableau
    tableau7 : Tableau
    heartsFoundation : Foundation
    spadesFoundation : Foundation
    diamondsFoundation : Foundation
    clubsFoundation : Foundation
    moving : MovingModel option
    pendingMove : MoveModel option
    unstaging : UnstagingModel option
    rng : System.Random
    popReady : bool
    pendingGestures : PendingGesture list
    previousTouches : Touch list
    }


type DealState = {
    tableauDealMoves : (TableauNumber * bool) list
    model : Model
    }



//
// --------- Functions ---------
//

let suit ( Card ( suit , _ ) ) = suit


let face ( Card ( _ , face ) ) = face


let suits = [ Hearts; Clubs; Diamonds; Spades ]


let faces = [ KeySignature; Do; Re; Mi; Fa; So; La; Ti; Do8; IV; V; I ]


let tableauNumber = function
| 1 -> Tableau1
| 2 -> Tableau2
| 3 -> Tableau3
| 4 -> Tableau4
| 5 -> Tableau5
| 6 -> Tableau6
| 7 -> Tableau7
| _ -> raise (System.ArgumentOutOfRangeException())


let faceUp (Tableau (up,_)) = up


let faceDown (Tableau (_,down)) = down


let initTableau = Tableau ([],[])


let initFoundation suit = Foundation ( [] , suit )


let getTableauDealMoves state = state.tableauDealMoves


let getModel state = state.model


let setTableauDealMoves tableauDealMoves state = { state with tableauDealMoves = tableauDealMoves }


let setModel model (state : DealState) = { state with model = model }


let setTableau tableau newTableau model = 
    match tableau with
    | Tableau1 -> { model with tableau1 = newTableau }
    | Tableau2 -> { model with tableau2 = newTableau }
    | Tableau3 -> { model with tableau3 = newTableau }
    | Tableau4 -> { model with tableau4 = newTableau }
    | Tableau5 -> { model with tableau5 = newTableau }
    | Tableau6 -> { model with tableau6 = newTableau }
    | Tableau7 -> { model with tableau7 = newTableau }


let getTableau tableau model = 
    match tableau with
    | Tableau1 -> model.tableau1
    | Tableau2 -> model.tableau2
    | Tableau3 -> model.tableau3
    | Tableau4 -> model.tableau4
    | Tableau5 -> model.tableau5
    | Tableau6 -> model.tableau6
    | Tableau7 -> model.tableau7


let modifyTableau tableau f model = getTableau tableau model |> f |> (flip (setTableau tableau)) model


let setFoundation suit newCards model  =
    match suit with
    | Hearts -> { model with heartsFoundation = Foundation ( newCards , suit ) }
    | Spades -> { model with spadesFoundation = Foundation ( newCards , suit ) }
    | Diamonds -> { model with diamondsFoundation = Foundation ( newCards , suit ) }
    | Clubs -> { model with clubsFoundation = Foundation ( newCards , suit ) }


let getFoundation suit model =
    match suit with
    | Hearts -> model.heartsFoundation
    | Spades -> model.spadesFoundation
    | Diamonds -> model.diamondsFoundation
    | Clubs -> model.clubsFoundation


let cardsInFoundation (Foundation ( cards , _ )) = cards


let modifyFoundation suit f model = 
    let (Foundation ( cards , _ )) = getFoundation suit model
    f cards |> (flip (setFoundation suit)) model


let modifyTalon f model = { model with talon = f model.talon }


let modifyStock f model = { model with stock = f model.stock }


let pushCardToFoundation foundation card = modifyFoundation foundation (card |> applyT2 List.Cons)


let pushCardToTalon card = modifyTalon (card |> applyT2 List.Cons)


let pushCardToStock card = modifyStock (card |> applyT2 List.Cons)


let updateTableau faceUp card (Tableau (up,down)) = Tableau (if faceUp then card::up,down else up,card::down)


let pushCardToTableau tableau faceUp card = updateTableau faceUp card |> modifyTableau tableau


let setTableauFaceUp tableau model newFaceUp =
    modifyTableau 
        tableau
        (fun (Tableau (_,down)) -> Tableau (newFaceUp,down))
        model


let modifyTableauFaceUp tableau f model = getTableau tableau model |> faceUp |> f |> setTableauFaceUp tableau model


let asTableauModifier f (Tableau (up, down)) = (up, down) |> f |> Tableau


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


let canPlaceOnFoundation foundation ( Card ( suit , face ) ) =
    match foundation with
    | Foundation ( [] , requiredSuit ) -> face = KeySignature && suit = requiredSuit
    | Foundation ( Card ( _ , targetFace ) ::_ , requiredSuit ) -> isFace2Higher targetFace face && suit = requiredSuit


let canPlaceOnTableau tableau cards =
    match tableau,(List.rev cards) with
    | [],(Card (_,I) :: _) -> true
    | (Card (targetSuit, targetFace) :: _),(Card (suit,face) :: _) -> 
        isFace2Higher face targetFace && areAlternateSuits targetSuit suit 
    | _ -> false


let replenish = asTableauModifier (function | [],(head :: tail) -> [head],tail | x -> x)