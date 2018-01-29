module Model

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
    | Tableau1
    | Tableau2
    | Tableau3
    | Tableau4
    | Tableau5
    | Tableau6
    | Tableau7
    | HeartsFoundation
    | SpadesFoundation
    | DiamondsFoundation
    | ClubsFoundation

type DealPhaseModel = {
    deck : Card list
    tableauDealMoves : (int*bool) list
    }

type Phase =
    | DealPhase of DealPhaseModel
    | PlayingPhase
    | WonPhase

type MovingModel = Pile * Card list * (float * float) * string option

type Model = {
    phase : Phase
    stock : Card list
    talon : Card list
    tableau1 : Card list
    tableau2 : Card list * Card list
    tableau3 : Card list * Card list
    tableau4 : Card list * Card list
    tableau5 : Card list * Card list
    tableau6 : Card list * Card list
    tableau7 : Card list * Card list
    heartsFoundation : Card list
    spadesFoundation : Card list
    diamondsFoundation : Card list
    clubsFoundation : Card list
    moving : MovingModel option
    rng : System.Random
    popReady : bool
    }