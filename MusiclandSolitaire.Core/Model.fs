module Model

open FsGame.Core

open FSharpPlus
open Aether
open Aether.Operators
open FSharpPlus.Data


//
// --------- Non-Domain ---------
//

let _1of2 : Lens<'a * 'b, 'a> = (fun (a,_) -> a) , (fun a (_,b) -> (a,b))
let _2of2 : Lens<'a * 'b, 'b> = (fun (_,b) -> b) , (fun b (a,_) -> (a,b))

let _1of3 : Lens<'a * 'b * 'c, 'a> = (fun (a,_,_) -> a) , (fun a (_,b,c) -> (a,b,c))
let _2of3 : Lens<'a * 'b * 'c, 'b> = (fun (_,b,_) -> b) , (fun b (a,_,c) -> (a,b,c))
let _3of3 : Lens<'a * 'b * 'c, 'c> = (fun (_,_,c) -> c) , (fun c (a,b,_) -> (a,b,c))

let _1of4 : Lens<'a * 'b * 'c * 'd, 'a> = (fun (a,_,_,_) -> a) , (fun a (_,b,c,d) -> (a,b,c,d))
let _2of4 : Lens<'a * 'b * 'c * 'd, 'b> = (fun (_,b,_,_) -> b) , (fun b (a,_,c,d) -> (a,b,c,d))
let _3of4 : Lens<'a * 'b * 'c * 'd, 'c> = (fun (_,_,c,_) -> c) , (fun c (a,b,_,d) -> (a,b,c,d))
let _4of4 : Lens<'a * 'b * 'c * 'd, 'd> = (fun (_,_,_,d) -> d) , (fun d (a,b,c,_) -> (a,b,c,d))


let _1of5 : Lens<'a * 'b * 'c * 'd * 'e, 'a> = (fun (a,_,_,_,_) -> a) , (fun a (_,b,c,d,e) -> (a,b,c,d,e))
let _2of5 : Lens<'a * 'b * 'c * 'd * 'e, 'b> = (fun (_,b,_,_,_) -> b) , (fun b (a,_,c,d,e) -> (a,b,c,d,e))
let _3of5 : Lens<'a * 'b * 'c * 'd * 'e, 'c> = (fun (_,_,c,_,_) -> c) , (fun c (a,b,_,d,e) -> (a,b,c,d,e))
let _4of5 : Lens<'a * 'b * 'c * 'd * 'e, 'd> = (fun (_,_,_,d,_) -> d) , (fun d (a,b,c,_,e) -> (a,b,c,d,e))
let _5of5 : Lens<'a * 'b * 'c * 'd * 'e, 'e> = (fun (_,_,_,_,e) -> e) , (fun e (a,b,c,d,_) -> (a,b,c,d,e))

let _1of6 : Lens<'a * 'b * 'c * 'd * 'e * 'f, 'a> = (fun (a,_,_,_,_,_) -> a) , (fun a (_,b,c,d,e,f) -> (a,b,c,d,e,f))
let _2of6 : Lens<'a * 'b * 'c * 'd * 'e * 'f, 'b> = (fun (_,b,_,_,_,_) -> b) , (fun b (a,_,c,d,e,f) -> (a,b,c,d,e,f))
let _3of6 : Lens<'a * 'b * 'c * 'd * 'e * 'f, 'c> = (fun (_,_,c,_,_,_) -> c) , (fun c (a,b,_,d,e,f) -> (a,b,c,d,e,f))
let _4of6 : Lens<'a * 'b * 'c * 'd * 'e * 'f, 'd> = (fun (_,_,_,d,_,_) -> d) , (fun d (a,b,c,_,e,f) -> (a,b,c,d,e,f))
let _5of6 : Lens<'a * 'b * 'c * 'd * 'e * 'f, 'e> = (fun (_,_,_,_,e,_) -> e) , (fun e (a,b,c,d,_,f) -> (a,b,c,d,e,f))
let _6of6 : Lens<'a * 'b * 'c * 'd * 'e * 'f, 'f> = (fun (_,_,_,_,_,f) -> f) , (fun f (a,b,c,d,e,_) -> (a,b,c,d,e,f))

let pointX : Lens<Point, float> = (fun (Point (x, _)) -> x) , (fun x (Point (_, y)) -> Point (x,y))
let pointY : Lens<Point, float> = (fun (Point (_, y)) -> y) , (fun y (Point (x, _)) -> Point (x,y))

let gameTimeElapsed : Lens<GameTime,System.TimeSpan> = (fun gt -> gt.elapsed) , (fun e gt -> { gt with elapsed = e })
let gameTimeIsRunningSlowly : Lens<GameTime,bool> = (fun gt -> gt.isRunningSlowly) , (fun isRunningSlowly gt -> { gt with isRunningSlowly = isRunningSlowly })
let gameTimeTotal : Lens<GameTime,System.TimeSpan> = (fun gt -> gt.total) , (fun total gt -> { gt with total = total })

let optionSome : Prism<'a option, 'a> = (id , (fun x -> Some x |> konst))

let extractL (lens : Lens<'a, 'b>) (f : 'b -> 'c) = Optic.get lens >> f
let extractP (prism : Prism<'a, 'b>) (f : 'b -> 'c) = Optic.get prism >> map f
let setAndExtractL (lens : Lens<'a, 'b>) (State f : State<'b, 'c>) = (fun a -> Optic.get lens a |> f |> second (flip (Optic.set lens) a)) |> State
let setAndExtractP (prism : Prism<'a, 'b>) (State f : State<'b, 'c>) =
    (fun a ->
        Optic.get prism a
        |> map f
        |> (function
            | Some (c, b) -> (Some c, Optic.set prism b a)
            | None -> (None, a)))
    |> State

let clearL lens  = setAndExtractL lens (State (function (Some x) -> (Some x, None) | None -> (None, None)))

type StateOptionBuilder() =

    member inline this.Bind(State s, f) = State (s >> (function (Some x, st) -> f x |> (fun (State s') -> s' st) | (None, st) -> (None, st)))

    member inline this.Return x = State (fun st -> (Some x, st))

let stateOption = StateOptionBuilder()

let liftOption (op : 'a option) : State<'b, 'a option> = State (fun st -> (op, st))

let liftState (State s) = State (s >> first Some)

let modifyState (f : 'a -> 'a) : State<'a, unit> =
    monad {
        let! s = get
        do! put (f s)
    }

open FsGame.Touch

let cons x xs = x :: xs

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

let private cardTuple : Lens<Card, Suit * Face>  = (fun (Card (s,f)) -> (s,f)), (fun tuple' (Card _) -> Card tuple')
let cardSuit = cardTuple >-> _1of2
let cardFace = cardTuple >-> _2of2



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

let triginFoundation : Prism<MoveOrigin, Suit> = (function Foundation s -> Some s | _ -> None) , (fun s _ -> Foundation s)
let triginTableau : Prism<MoveOrigin, TableauNumber> = (function Tableau t -> Some t | _ -> None) , (fun t _ -> Tableau t)



type MoveTarget =
    | Foundation of Suit
    | Tableau of TableauNumber

let targetFoundationSuit : Prism<MoveTarget, Suit> = (function Foundation s -> Some s | _ -> None) , (fun s _ -> Foundation s)
let targetTableauNumber : Prism<MoveTarget, TableauNumber> = (function Tableau t -> Some t | _ -> None) , (fun t _ -> Tableau t)



type Tableau = Tableau of Card list * Card list

let private tableauTuple : Lens<Tableau, Card list * Card list>  = (fun (Tableau (x,y)) -> (x,y)), (fun tuple' (Tableau _) -> Tableau tuple')
let tableauFaceUp = tableauTuple >-> _1of2
let tableauFaceDown = tableauTuple >-> _2of2



type Foundation = Foundation of Card list * Suit

let private foundationTuple : Lens<Foundation, Card list * Suit>  = (fun (Foundation (x,y)) -> (x,y)), (fun tuple' (Foundation _) -> Foundation tuple')
let foundationCards = foundationTuple >-> _1of2
let foundationSuit = foundationTuple >-> _2of2



type StagedModel = StagedModel of MoveTarget * Point * Point * float * GameTime * bool

let private stagedTuple : Lens<StagedModel, MoveTarget * Point * Point * float * GameTime * bool> =
    (fun (StagedModel (a,b,c,d,e,f)) -> (a,b,c,d,e,f)), (fun tuple (StagedModel _) -> StagedModel tuple)

let stagedTarget = stagedTuple >-> _1of6
let stagedOrigin = stagedTuple >-> _2of6
let stagedDest = stagedTuple >-> _3of6
let stagedProgress = stagedTuple >-> _4of6
let stagedTimeStaged = stagedTuple >-> _5of6
let stagedPlayedSound = stagedTuple >-> _6of6

let stagedTargetFoundationSuit = stagedTarget >-> targetFoundationSuit
let stagedTargetTableauNumber = stagedTarget >-> targetTableauNumber
let stagedOriginX = stagedOrigin >-> pointX
let stagedOriginY = stagedOrigin >-> pointY
let stagedDestX = stagedDest >-> pointX
let stagedDestY = stagedDest >-> pointY
let stagedTimeStagedElapsed = stagedTimeStaged >-> gameTimeElapsed
let stagedTimeStagedIsRunningSlowly = stagedTimeStaged >-> gameTimeIsRunningSlowly
let stagedTimeStagedTotal = stagedTimeStaged >-> gameTimeTotal




type UnstagingModel = UnstagingModel of Point * float

let private unstagingTuple : Lens<UnstagingModel, Point * float> =
    (fun (UnstagingModel (a,b)) -> (a,b)), (fun tuple (UnstagingModel _) -> UnstagingModel tuple)

let unstagingDest = unstagingTuple >-> _1of2
let unstagingProgress = unstagingTuple >-> _2of2

let unstagingDestX = unstagingDest >-> pointX
let unstagingDestY = unstagingDest >-> pointY



type Staging =
    | Staged of StagedModel
    | Unstaging of UnstagingModel

let staged : Prism<Staging, StagedModel> =
    (function Staged s -> Some s | _ -> None) , (fun s _ -> Staged s)
let unstaging : Prism<Staging,UnstagingModel> =
    (function Unstaging u -> Some u | _ -> None), (fun u _ -> Unstaging u)



type Hand = MoveOrigin * Card list * Point * int * Staging option

let handOrigin : Lens<Hand, MoveOrigin>= _1of5
let handCards : Lens<Hand, Card list>= _2of5
let handLocation : Lens<Hand, Point>= _3of5
let handTouchId : Lens<Hand, int>= _4of5
let handStaging : Lens<Hand, Staging option>= _5of5



type PlayingModel = {
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
    hand : Hand option
    popReady : bool
    }

let modelTalon : Lens<PlayingModel, Card list> = (fun m -> m.talon) , (fun t m -> { m with talon = t })
let modelStock : Lens<PlayingModel, Card list> = (fun m -> m.stock) , (fun t m -> { m with stock = t })
let modelFoundation : Suit -> Lens<PlayingModel, Foundation> = function
| Hearts -> (fun m -> m.heartsFoundation) , (fun (Foundation (cards,_)) m -> { m with heartsFoundation = Foundation (cards,Hearts) })
| Spades -> (fun m -> m.spadesFoundation) , (fun (Foundation (cards,_)) m -> { m with spadesFoundation = Foundation (cards,Spades) })
| Diamonds -> (fun m -> m.diamondsFoundation) , (fun (Foundation (cards,_)) m -> { m with diamondsFoundation = Foundation (cards,Diamonds) })
| Clubs -> (fun m -> m.clubsFoundation) , (fun (Foundation (cards,_)) m -> { m with clubsFoundation = Foundation (cards,Clubs) })
let modelTableau : TableauNumber -> Lens<PlayingModel, Tableau> = function
| Tableau1 -> (fun m -> m.tableau1) , (fun t m -> { m with tableau1 = t })
| Tableau2 -> (fun m -> m.tableau2) , (fun t m -> { m with tableau2 = t })
| Tableau3 -> (fun m -> m.tableau3) , (fun t m -> { m with tableau3 = t })
| Tableau4 -> (fun m -> m.tableau4) , (fun t m -> { m with tableau4 = t })
| Tableau5 -> (fun m -> m.tableau5) , (fun t m -> { m with tableau5 = t })
| Tableau6 -> (fun m -> m.tableau6) , (fun t m -> { m with tableau6 = t })
| Tableau7 -> (fun m -> m.tableau7) , (fun t m -> { m with tableau7 = t })
let modelHand : Lens<PlayingModel, Hand option> = (fun m -> m.hand) , (fun h m -> { m with hand = h })


type DealState = {
    tableauDealMoves : (TableauNumber * bool) list
    model : PlayingModel
    }


type GameState =
    | Playing of PlayingModel


type Model = {
    pendingGestures : PendingGesture list
    previousTouches : Touch list
    rng : System.Random
    gameState : GameState
    }


let modelPlaying : Prism<Model, PlayingModel> = (function { gameState = Playing p } -> Some p | _ -> None), (fun p m -> { m with gameState = Playing p })



//
// --------- Msg ---------
//

type TagId =
    | Nothing
    | Background
    | FlipTalon
    | Reset
    | MovingBottom of Card
    | Target of MoveTarget

type PlayingMsg =
    | PreparePop
    | PopStock
    | BeginMove of MoveOrigin * int * Point * int
    | Move of int * Point
    | CancelMove
    | StageMove of MoveTarget * Point
    | UnstageMove
    | CommitMove of int
    | MoveCommitted
    | CardTapped of Card
    | FlipTalon
    | PlayMoveSound of Msg list

and Msg =
    | Step
    | Reset
    | PlayingMsg of PlayingMsg

type Tag = {
    id : TagId
    position : Point
    tapHandler : (int -> Point -> Msg) option
    touchDownHandler : (int -> Point -> Msg) option
    touchUpHandler : (int -> Point -> Msg) option
    dragHandler : (int -> Delta -> Point -> Msg) option
    stopTouchPropagation : bool
    overlapHandler : (Overlap -> Msg option) option
}
and Overlap = Overlap of Tag * BoundingBox

type Operation =
    | Msg of Msg
    | Cmd of Cmd<Model, Touch list, Tag>



//
// --------- Functions ---------
//


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


let initTableau = Tableau ([],[])


let initFoundation suit = Foundation ( [] , suit )


let pushCardToFoundation = Optic.map foundationCards << cons


let pushCardToTalon = Optic.map modelTalon << cons



let pushCardToStock = Optic.map modelStock << cons


let updateTableau faceUp card (Tableau (up,down)) = Tableau (if faceUp then card::up,down else up,card::down)


let pushCardToTableau tableau up = Optic.map (modelTableau tableau >-> if up then tableauFaceUp else tableauFaceDown) << cons


let getTopCard target =
    match target with
    | MoveTarget.Foundation f -> Optic.get ((modelFoundation f) >-> foundationCards)
    | MoveTarget.Tableau t -> Optic.get ((modelTableau t) >-> tableauFaceUp)
    >> List.tryHead


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