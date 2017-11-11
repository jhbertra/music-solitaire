module Game



//
// --------- Model ---------
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
    | Me
    | Fa
    | So
    | La
    | Ti
    | Do8
    | IV
    | V
    | I

let suits = [ Hearts; Clubs; Diamonds; Spades ]
let faces = 
    [ 
    KeySignature
    Do
    Re
    Me
    Fa
    So
    La
    Ti
    Do8
    IV
    V
    I
    ]

type Card = Card of Suit * Face

type TableauNumber =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven

type TableauState =
    | FaceDown
    | FaceUp

type Location =
    | Tableau of TableauNumber * TableauState
    | Foundation of Suit
    | Stock
    | Waste

type DealtCard = DealtCard of Card * Location

type Deck = Deck of DealtCard list

type DealState = DealState of Card list * Deck * Location * TableauNumber



//
// --------- Init ---------
//

let initDeck = 
    [for suit in suits do
        for face in faces do
            yield Card (suit, face)]

let shuffle (rng: System.Random) arr =
    let array = Array.copy arr
    let n = array.Length
    for x in 1..n do
        let i = n-x
        let j = rng.Next(i+1)
        let tmp = array.[i]
        array.[i] <- array.[j]
        array.[j] <- tmp
    array

let nextTableauNumber number =
    match number with
    | One -> Two
    | Two -> Three
    | Three -> Four
    | Four -> Five
    | Five -> Six
    | Six -> Seven
    | Seven -> Seven

let nextLocation current tableauPass =
    match current with
    | Stock | Tableau (Seven, FaceUp) -> (Stock, Seven)
    | Tableau (Seven, FaceDown) -> (Tableau (nextTableauNumber tableauPass, FaceUp), nextTableauNumber tableauPass)
    | Tableau (n, f) -> (Tableau (nextTableauNumber n, FaceDown), tableauPass)
    | _ -> (current, tableauPass)

let rec deal dealState =
    let (cards, deck, location, tableauPass) = match dealState with DealState (c,d,l,p) -> (c,d,l,p)
    let cardsInDeck = match deck with Deck c -> c
    match cards with
    | [] -> deck
    | card :: remaining -> 
        let (newLocation, newTableauPass) = nextLocation location tableauPass
        deal (DealState (remaining, Deck (DealtCard (card, location) :: cardsInDeck), newLocation, newTableauPass))

let dealDeck rng =
    let cards = 
        initDeck
        |> Array.ofList
        |> shuffle rng
        |> List.ofArray
    deal (DealState (cards, Deck [], Tableau (One, FaceUp), One))