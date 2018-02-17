module Model

open Core
open Touch

//
// --------- Model ---------
//

type Suit =
    | Hearts
    | Clubs
    | Diamonds
    | Spades
let suits = [ Hearts; Clubs; Diamonds; Spades ]

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
let faces = [ KeySignature; Do; Re; Mi; Fa; So; La; Ti; Do8; IV; V; I ]

type Card = Suit * Face

type Pile =
    | Stock
    | Talon
    | HeartsFoundation
    | SpadesFoundation
    | DiamondsFoundation
    | ClubsFoundation

type TableauNumber =
    | Tableau1
    | Tableau2
    | Tableau3
    | Tableau4
    | Tableau5
    | Tableau6
    | Tableau7

let tableauNumber = function
| 1 -> Tableau1
| 2 -> Tableau2
| 3 -> Tableau3
| 4 -> Tableau4
| 5 -> Tableau5
| 6 -> Tableau6
| 7 -> Tableau7
| _ -> raise (System.ArgumentOutOfRangeException())

type Tableau = Tableau of Card list * Card list

let faceUp (Tableau (up,_)) = up

type MoveOrigin =
    | Pile of Pile
    | Tableau of TableauNumber

type MovingModel = MoveOrigin * Card list * Point * string option * int

type MoveModel = MoveModel of MoveOrigin * Point * Point * float

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
    heartsFoundation : Card list
    spadesFoundation : Card list
    diamondsFoundation : Card list
    clubsFoundation : Card list
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

let initPile = []

let initTableau = Tableau.Tableau ([],[])

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

let setPile pile newCards model  =
    match pile with
    | Stock -> { model with stock = newCards }
    | Talon -> { model with talon = newCards }
    | HeartsFoundation -> { model with heartsFoundation = newCards }
    | SpadesFoundation -> { model with spadesFoundation = newCards }
    | DiamondsFoundation -> { model with diamondsFoundation = newCards }
    | ClubsFoundation -> { model with clubsFoundation = newCards }

let getPile pile model =
    match pile with
    | Stock -> model.stock
    | Talon -> model.talon
    | HeartsFoundation -> model.heartsFoundation
    | SpadesFoundation -> model.spadesFoundation
    | DiamondsFoundation -> model.diamondsFoundation
    | ClubsFoundation -> model.clubsFoundation

let modifyPile pile f model = getPile pile model |> f |> (flip (setPile pile)) model

let pushCardToPile pile card = modifyPile pile (card |> applyT2 List.Cons)

let updateTableau faceUp card (Tableau.Tableau (up,down)) = Tableau.Tableau (if faceUp then card::up,down else up,card::down)

let pushCardToTableau tableau faceUp card = updateTableau faceUp card |> modifyTableau tableau

let setTableauFaceUp tableau model newFaceUp =
    modifyTableau 
        tableau
        (fun (Tableau.Tableau (_,down)) -> Tableau.Tableau (newFaceUp,down))
        model

let modifyTableauFaceUp tableau f model = getTableau tableau model |> faceUp |> f |> setTableauFaceUp tableau model

let asTableauModifier f (Tableau.Tableau (up, down)) = (up, down) |> f |> Tableau.Tableau

let getSuit = function
| HeartsFoundation -> Hearts
| SpadesFoundation -> Spades
| DiamondsFoundation -> Diamonds
| ClubsFoundation -> Clubs
| _ -> raise (System.ArgumentOutOfRangeException())



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

let replenish = asTableauModifier (function | [],(head :: tail) -> [head],tail | x -> x)