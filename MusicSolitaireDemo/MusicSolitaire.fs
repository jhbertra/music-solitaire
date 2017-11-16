module MusicSolitaire

// ***************************************** //
//                  CORE                     //
// ***************************************** //

// --------- State ---------
//

type State<'s,'v> = State of ('s -> 'v * 's)

let runState state initial = match state with State f -> f initial

type StateBuilder() =

    member inline this.Return(x) = State (fun s -> x,s)

    member inline this.ReturnFrom(m) = m

    member inline this.Bind(m, f) = 
        State (fun s ->
            let (v, newState) = runState m s
            runState (f v) newState)

    member inline this.Zero() = State(fun s -> (),s)

let state = new StateBuilder()

let getState = State (fun s -> (s,s))

let putState newState = State (fun s -> ((), newState))

let modify f =
    state {
        let! x = getState
        do! putState (f x)
    }

let gets f =
    state {
        let! x = getState
        return f x
    }

let evalState state initial = runState state initial |> fst

let execState state initial = runState state initial |> snd



//
// --------- Other Stuff ---------
//

type Cmd<'a> =
    | Term
    | Msg of 'a

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





// ***************************************** //
//                  GAME                     //
// ***************************************** //

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

type MovingModel = Pile * Card list

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
    }



//
// --------- Msg ---------
//

type Msg =
    | DealCards
    | PopStock
    | BeginMove of Pile * int
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
    | [_,targetFace],(suit,face) -> isFace2Higher targetFace face && suit = requiredSuit
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

    | PlayingPhase,(BeginMove (origin, count)) ->
        match model.moving,origin,count with
        | None,Talon,1 when List.length model.talon > 0 ->
            { model with
                talon = List.skip 1 model.talon
                moving = Some (origin,(List.take 1 model.talon))
                }
                ,Term
        | None,HeartsFoundation,1 when List.length model.heartsFoundation > 0 ->
            { model with
                heartsFoundation = List.skip 1 model.heartsFoundation
                moving = Some (origin,(List.take 1 model.heartsFoundation))
                }
                ,Term
        | None,SpadesFoundation,1 when List.length model.spadesFoundation > 0 ->
            { model with
                spadesFoundation = List.skip 1 model.spadesFoundation
                moving = Some (origin,(List.take 1 model.spadesFoundation))
                }
                ,Term
        | None,DiamondsFoundation,1 when List.length model.diamondsFoundation > 0 ->
            { model with
                diamondsFoundation = List.skip 1 model.diamondsFoundation
                moving = Some (origin,(List.take 1 model.diamondsFoundation))
                }
                ,Term
        | None,ClubsFoundation,1 when List.length model.clubsFoundation > 0 ->
            { model with
                clubsFoundation = List.skip 1 model.clubsFoundation
                moving = Some (origin,(List.take 1 model.clubsFoundation))
                }
                ,Term
        | None,Tableau1,n when n <= List.length model.tableau1 && n > 0 ->
            { model with
                tableau1 = List.skip n model.tableau1
                moving = Some (origin,(List.take n model.tableau1))
                }
                ,Term
        | None,Tableau2,n when n <= List.length (fst model.tableau2) && n > 0 ->
            { model with
                tableau2 = (List.skip n (fst model.tableau2)),(snd model.tableau2)
                moving = Some (origin,(List.take n (fst model.tableau2)))
                }
                ,Term
        | None,Tableau3,n when n <= List.length (fst model.tableau3) && n > 0 ->
            { model with
                tableau3 = (List.skip n (fst model.tableau3)),(snd model.tableau3)
                moving = Some (origin,(List.take n (fst model.tableau3)))
                }
                ,Term
        | None,Tableau4,n when n <= List.length (fst model.tableau4) && n > 0 ->
            { model with
                tableau4 = (List.skip n (fst model.tableau4)),(snd model.tableau4)
                moving = Some (origin,(List.take n (fst model.tableau4)))
                }
                ,Term
        | None,Tableau5,n when n <= List.length (fst model.tableau5) && n > 0 ->
            { model with
                tableau5 = (List.skip n (fst model.tableau5)),(snd model.tableau5)
                moving = Some (origin,(List.take n (fst model.tableau5)))
                }
                ,Term
        | None,Tableau6,n when n <= List.length (fst model.tableau6) && n > 0 ->
            { model with
                tableau6 = (List.skip n (fst model.tableau6)),(snd model.tableau6)
                moving = Some (origin,(List.take n (fst model.tableau6)))
                }
                ,Term
        | None,Tableau7,n when n <= List.length (fst model.tableau7) && n > 0 ->
            { model with
                tableau7 = (List.skip n (fst model.tableau7)),(snd model.tableau7)
                moving = Some (origin,(List.take n (fst model.tableau7)))
                }
                ,Term
        | _ -> model,Term

    | PlayingPhase,CancelMove ->
        match model.moving with
        | Some (Talon,cards) -> 
            { model with
                talon = cards @ model.talon
                moving = None
                }
                ,Term
        | Some (HeartsFoundation,cards) -> 
            { model with
                heartsFoundation = cards @ model.heartsFoundation
                moving = None
                }
                ,Term
        | Some (SpadesFoundation,cards) -> 
            { model with
                spadesFoundation = cards @ model.spadesFoundation
                moving = None
                }
                ,Term
        | Some (DiamondsFoundation,cards) -> 
            { model with
                diamondsFoundation = cards @ model.diamondsFoundation
                moving = None
                }
                ,Term
        | Some (ClubsFoundation,cards) -> 
            { model with
                clubsFoundation = cards @ model.clubsFoundation
                moving = None
                }
                ,Term
        | Some (Tableau1,cards) -> 
            { model with
                tableau1 = cards @ model.tableau1
                moving = None
                }
                ,Term
        | Some (Tableau2,cards) -> 
            { model with
                tableau2 = (cards @ (fst model.tableau2),(snd model.tableau2))
                moving = None
                }
                ,Term
        | Some (Tableau3,cards) -> 
            { model with
                tableau3 = (cards @ (fst model.tableau3),(snd model.tableau3))
                moving = None
                }
                ,Term
        | Some (Tableau4,cards) -> 
            { model with
                tableau4 = (cards @ (fst model.tableau4),(snd model.tableau4))
                moving = None
                }
                ,Term
        | Some (Tableau5,cards) -> 
            { model with
                tableau5 = (cards @ (fst model.tableau5),(snd model.tableau5))
                moving = None
                }
                ,Term
        | Some (Tableau6,cards) -> 
            { model with
                tableau6 = (cards @ (fst model.tableau6),(snd model.tableau6))
                moving = None
                }
                ,Term
        | Some (Tableau7,cards) -> 
            { model with
                tableau7 = (cards @ (fst model.tableau7),(snd model.tableau7))
                moving = None
                }
                ,Term
        | _ -> model,Term

    | PlayingPhase,(CommitMove target) ->
        match model.moving with
        | None -> model,Term
        | Some (_,cards) ->
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
            | [card],DiamondsFoundation when canPlaceOnFoundation model.heartsFoundation Diamonds card ->
                { model with
                    diamondsFoundation = card :: model.diamondsFoundation
                    moving = None
                    }
                    ,(Msg MoveCommitted)
            | [card],ClubsFoundation when canPlaceOnFoundation model.heartsFoundation Clubs card ->
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
                tableau4 = replenish model.tableau3
                tableau3 = replenish model.tableau4
                tableau5 = replenish model.tableau5
                tableau6 = replenish model.tableau6
                tableau7 = replenish model.tableau7
                }
                ,Term

    | _,Reset -> initModel model.rng

    | _ -> model,Term






// ***************************************** //
//                PLATFORM                   //
// ***************************************** //

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input.Touch

type MusicSolitaireGame() as this =
    inherit Game()

    let manager = new GraphicsDeviceManager(this)
    let model,initialCmd = initModel(new System.Random())

    [<DefaultValue>] val mutable model : Model

    [<DefaultValue>] val mutable background : Graphics.Texture2D
    [<DefaultValue>] val mutable cardBack : Graphics.Texture2D
    [<DefaultValue>] val mutable keySignature : Graphics.Texture2D
    [<DefaultValue>] val mutable doe : Graphics.Texture2D
    [<DefaultValue>] val mutable re : Graphics.Texture2D
    [<DefaultValue>] val mutable mi : Graphics.Texture2D
    [<DefaultValue>] val mutable fa : Graphics.Texture2D
    [<DefaultValue>] val mutable so : Graphics.Texture2D
    [<DefaultValue>] val mutable la : Graphics.Texture2D
    [<DefaultValue>] val mutable ti : Graphics.Texture2D
    [<DefaultValue>] val mutable do8 : Graphics.Texture2D
    [<DefaultValue>] val mutable iv : Graphics.Texture2D
    [<DefaultValue>] val mutable v : Graphics.Texture2D
    [<DefaultValue>] val mutable i : Graphics.Texture2D
    [<DefaultValue>] val mutable hearts : Graphics.Texture2D
    [<DefaultValue>] val mutable spades : Graphics.Texture2D
    [<DefaultValue>] val mutable diamonds : Graphics.Texture2D
    [<DefaultValue>] val mutable clubs : Graphics.Texture2D
    [<DefaultValue>] val mutable spriteBatch : Graphics.SpriteBatch

    let rec runCmd cmd = 
        state {
            let! model = getState
            match cmd with
            | Term -> return model
            | Msg msg -> 
                let newModel,nextCmd = update msg model
                do! putState newModel
                return! runCmd nextCmd
            }

    let execCmd cmd model =
        let gameStateBuilder = runCmd cmd
        execState gameStateBuilder model
    //
    // --------- Initialize ---------
    //

    override __.Initialize() = 
        this.spriteBatch <- new Graphics.SpriteBatch(this.GraphicsDevice)
        this.model <- execCmd initialCmd model
        TouchPanel.EnabledGestures <- GestureType.Tap
        base.Initialize()

    //
    // --------- Load ---------
    //
        

    override __.LoadContent() =
        this.Content.RootDirectory <- "Content"
        this.background <- this.Content.Load<Graphics.Texture2D>("Table")
        this.cardBack <- this.Content.Load<Graphics.Texture2D>("CardBack")
        this.keySignature <- this.Content.Load<Graphics.Texture2D>("Ks")
        this.doe <- this.Content.Load<Graphics.Texture2D>("Do")
        this.re <- this.Content.Load<Graphics.Texture2D>("Re")
        this.mi <- this.Content.Load<Graphics.Texture2D>("Mi")
        this.fa <- this.Content.Load<Graphics.Texture2D>("Fa")
        this.so <- this.Content.Load<Graphics.Texture2D>("So")
        this.la <- this.Content.Load<Graphics.Texture2D>("La")
        this.ti <- this.Content.Load<Graphics.Texture2D>("Ti")
        this.do8 <- this.Content.Load<Graphics.Texture2D>("Do8")
        this.iv <- this.Content.Load<Graphics.Texture2D>("IV")
        this.v <- this.Content.Load<Graphics.Texture2D>("V")
        this.i <- this.Content.Load<Graphics.Texture2D>("I")
        this.hearts <- this.Content.Load<Graphics.Texture2D>("Hearts")
        this.spades <- this.Content.Load<Graphics.Texture2D>("Spades")
        this.diamonds <- this.Content.Load<Graphics.Texture2D>("Diamonds")
        this.clubs <- this.Content.Load<Graphics.Texture2D>("Clubs")
        base.LoadContent()

    //
    // --------- Update ---------
    //

    let isInStock (tapPos : Vector2) =
        tapPos.X >= 638.0f && tapPos.X < 724.0f
        && tapPos.Y >= 26.0f && tapPos.Y < 141.0f

    override __.Update(gameTime) =
        while TouchPanel.IsGestureAvailable do
            let gesture = TouchPanel.ReadGesture()
            if gesture.GestureType = GestureType.Tap && isInStock gesture.Position
            then
                this.model <- execCmd (Msg PopStock) this.model
            else
                ()
        base.Update(gameTime)

    //
    // --------- Draw ---------
    //

    member __.DrawStock() =
        match this.model.stock with
        | [] -> ()
        | _ -> this.spriteBatch.Draw(this.cardBack, new Vector2(638.0f,26.0f), Color.White)

    member __.DrawTalon() =
        match this.model.talon with
        | [] -> ()
        | card::_ -> this.DrawCard(card, 536.0f, 26.0f)

    member __.DrawDown(x, y, down) =
        match down with
        | [] -> y
        | card :: cards ->
            let y = this.DrawDown(x, y, cards)
            this.spriteBatch.Draw(this.cardBack, new Vector2(x,y), Color.White)
            y + 32.0f

    member __.DrawCard(card, x, y) =
        match card with
        | _,KeySignature -> this.spriteBatch.Draw(this.keySignature, new Vector2(x,y), Color.White)
        | _,Do -> this.spriteBatch.Draw(this.doe, new Vector2(x,y), Color.White)
        | _,Re -> this.spriteBatch.Draw(this.re, new Vector2(x,y), Color.White)
        | _,Mi -> this.spriteBatch.Draw(this.mi, new Vector2(x,y), Color.White)
        | _,Fa -> this.spriteBatch.Draw(this.fa, new Vector2(x,y), Color.White)
        | _,So -> this.spriteBatch.Draw(this.so, new Vector2(x,y), Color.White)
        | _,La -> this.spriteBatch.Draw(this.la, new Vector2(x,y), Color.White)
        | _,Ti -> this.spriteBatch.Draw(this.ti, new Vector2(x,y), Color.White)
        | _,Do8 -> this.spriteBatch.Draw(this.do8, new Vector2(x,y), Color.White)
        | _,IV -> this.spriteBatch.Draw(this.iv, new Vector2(x,y), Color.White)
        | _,V -> this.spriteBatch.Draw(this.v, new Vector2(x,y), Color.White)
        | _,I -> this.spriteBatch.Draw(this.i, new Vector2(x,y), Color.White)

        match card with
        | Hearts,_ -> this.spriteBatch.Draw(this.hearts, new Vector2(x,y), Color.White)
        | Spades,_ -> this.spriteBatch.Draw(this.spades, new Vector2(x,y), Color.White)
        | Diamonds,_ -> this.spriteBatch.Draw(this.diamonds, new Vector2(x,y), Color.White)
        | Clubs,_ -> this.spriteBatch.Draw(this.clubs, new Vector2(x,y), Color.White)

    member __.DrawUp(x, y, up) =
        match List.rev up with
        | [] -> ()
        | card :: cards ->
            this.DrawCard(card, x, y)
            this.DrawUp(x, y + 32.0f, cards)

    member __.DrawTableau(num, down, up) =
        let x = 26.0f + ((float32 num) * 102.0f)
        let y = this.DrawDown(x, 193.0f, down)
        this.DrawUp(x, y, up)

    member __.DrawTableaus() =
        this.DrawTableau(0, [], this.model.tableau1)
        match this.model.tableau2 with up,down -> this.DrawTableau(1, down, up)
        match this.model.tableau3 with up,down -> this.DrawTableau(2, down, up)
        match this.model.tableau4 with up,down -> this.DrawTableau(3, down, up)
        match this.model.tableau5 with up,down -> this.DrawTableau(4, down, up)
        match this.model.tableau6 with up,down -> this.DrawTableau(5, down, up)
        match this.model.tableau7 with up,down -> this.DrawTableau(6, down, up)

    override __.Draw gameTime = 
        this.spriteBatch.Begin()
        this.spriteBatch.Draw(this.background, new Vector2(), Color.White)
        this.DrawStock()
        this.DrawTableaus()
        this.DrawTalon()
        this.spriteBatch.End()
        base.Draw(gameTime)