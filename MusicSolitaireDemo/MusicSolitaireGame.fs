﻿module MusicSolitaireGame

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Input.Touch

open Core
open Model
open Update
open View

type TouchLocationState =
    | Invalid
    | Moved
    | Pressed
    | Released

type TouchLocation = TouchLocation of int*TouchLocationState*(float*float)

type Touch = { position : float*float }
type Drag = { position : float*float; delta : float*float }

type TouchState =
    | TouchDown of Touch
    | TouchMoved of Drag
    | TouchUp of Touch

type Controller = (int*TouchState) list

type Event<'a> =
    | TimerEnd of Cmd<'a>

let updateController touchCol controller =
    let makeTouchState touchLocationState position prevPosition =
        match touchLocationState with
        | Pressed -> Some (TouchDown { position = position })
        | Released -> Some (TouchUp { position = position })
        | Moved -> Some(TouchMoved { position = position; delta = match position,prevPosition with (x2,y2),(x1,y1) -> (x2 - x1),(y2 - y1)})
        | _ -> None
    seq {
        for TouchLocation (id, state, position) in touchCol do
            let prevTouch = controller |> Seq.tryFind (fun (prevId,_) -> prevId = id)
            let newState =
                match prevTouch with
                | None -> makeTouchState state position position
                | Some (_,(TouchDown prevState)) | Some (_,(TouchUp prevState)) -> makeTouchState state position prevState.position
                | Some (_,(TouchMoved prevState)) -> makeTouchState state position prevState.position
            match newState with
            | None -> yield! []
            | Some newState -> yield id,newState            
    }
    |> List.ofSeq

let touchesEqual (cid,_) (TouchLocation (tid,_,_)) = cid = tid 

let droppedTouches touchCol controller =
    controller
    |> List.filter (fun (_,state) -> match state with TouchUp _ -> false | _ -> true)
    |> List.filter (fun touch -> not (touchCol |> List.ofSeq |> (List.exists (touchesEqual touch))))

let vector2ToFloatTuple (vec : Vector2) = ((float)vec.X,(float)vec.Y)

let translateTouchLocationState (state: Microsoft.Xna.Framework.Input.Touch.TouchLocationState) =
    match state with
    | Microsoft.Xna.Framework.Input.Touch.TouchLocationState.Moved -> Moved
    | Microsoft.Xna.Framework.Input.Touch.TouchLocationState.Pressed -> Pressed
    | Microsoft.Xna.Framework.Input.Touch.TouchLocationState.Released -> Released
    | _ -> Invalid
    
let getTouchState (touchCol: TouchCollection) =
    let scale = 750.0 / float GraphicsAdapter.DefaultAdapter.CurrentDisplayMode.Width // temporary hard-coded scaling
    touchCol
    |> Seq.map (fun touch -> TouchLocation (touch.Id,(translateTouchLocationState touch.State),((vector2ToFloatTuple >> mapT2 ((*) scale)) touch.Position)))

type MusicSolitaireGame() as this =
    inherit Game()

    let _ = new GraphicsDeviceManager(this)
    let model,initialCmd = initModel(System.Random())
    let events = System.Collections.Concurrent.ConcurrentQueue<Event<Msg>>()

    [<DefaultValue>] val mutable model : Model
    [<DefaultValue>] val mutable sprites : Sprite<Msg> list
    [<DefaultValue>] val mutable textures : Map<string, Texture2D>
    [<DefaultValue>] val mutable sfx : Map<string, SoundEffectInstance>
    [<DefaultValue>] val mutable controller : Controller
    [<DefaultValue>] val mutable spriteBatch : SpriteBatch


    let rec runCmd cmd = 
        state {
            let! model = getState
            match cmd with
            | Term -> return model
            | Msg msg -> 
                let newModel,nextCmd = update msg model
                do! putState newModel
                return! runCmd nextCmd
            | PlaySound(sound, mode, nextCmd) ->
                optional {
                    let! sfx = this.sfx.TryFind sound
                    match mode with
                    | Overlap -> sfx.Play() |> ignore
                    | NoOverlap ->
                        if sfx.State = SoundState.Stopped then
                            sfx.Play() |> ignore
                        else
                            () |> ignore
                } |> ignore
                return! runCmd nextCmd
            | Delay (delay,cmd) ->
                let timer = new System.Timers.Timer(1000.0 * delay)
                timer.AutoReset <- false
                timer.Enabled <- true
                timer.Elapsed.Add(fun _ -> events.Enqueue(TimerEnd cmd) |> ignore)
                timer.Start()
                return model
            }

    let execCmd cmd model =
        let gameStateBuilder = runCmd cmd
        execState gameStateBuilder model

    //
    // --------- Initialize ---------
    //

    override __.Initialize() = 
        this.spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        this.model <- execCmd initialCmd model
        this.sprites <- view this.model
        this.controller <- []
        TouchPanel.EnabledGestures <- GestureType.Tap ||| GestureType.FreeDrag ||| GestureType.DragComplete
        base.Initialize()

    //
    // --------- Load ---------
    //
        

    override __.LoadContent() =
        let manifest = contentManifest
        this.Content.RootDirectory <- manifest.root
        this.textures <- 
            manifest.textures
            |> List.fold (fun m t -> Map.add t (this.Content.Load<Texture2D>("textures/" + t)) m) (Map[])
        this.sfx <- 
            manifest.sfx
            |> List.fold (fun m t -> Map.add t (this.Content.Load<SoundEffect>("sfx/" + t).CreateInstance()) m) (Map[])
        base.LoadContent()

    //
    // --------- Update ---------
    //

    let positionInSprite (x,y) (sprite : Sprite<Msg>) =
        let spriteX,spriteY = sprite.position
        (x - spriteX),(y - spriteY)

    let spriteSize (sprite : Sprite<Msg>) =
        sprite.textures
        |> List.map (fun t -> Map.find t this.textures)
        |> List.fold (fun (width,height) t -> (max width ((float)t.Width)),(max height ((float)t.Height))) (0.0, 0.0)

    let rec msgFromGesture handler absPosition handlerPosition sprites =
        match sprites with
        | [] -> None
        | sprite :: tail ->
            match handler sprite with
            | None -> msgFromGesture handler absPosition handlerPosition tail
            | Some func ->
                let width,height = spriteSize sprite
                match positionInSprite absPosition sprite with
                | (x,y) when x > 0.0 && y > 0.0 && x <= width && y <= height -> Some (func handlerPosition)
                | _ -> msgFromGesture handler absPosition handlerPosition tail

    let handleGesture sprites model gesture =
        optional {
            let! msg = 
                match gesture with
                | TouchDown touch -> msgFromGesture (fun sprite -> sprite.touchDown) touch.position touch.position sprites
                | TouchMoved drag -> msgFromGesture (fun sprite -> sprite.touchMoved) drag.position drag.delta sprites
                | TouchUp touch -> msgFromGesture (fun sprite -> sprite.touchUp) touch.position touch.position sprites            
            return execCmd (Msg msg) model
        }
        |> defaultIfNone model       


    let handleInput sprites =
        List.fold (handleGesture sprites) this.model (List.map snd this.controller)

    let handleSub droppedTouches model sub =
        match sub with
        | TouchDropped msg ->
            if List.isEmpty droppedTouches then
                model
            else
                execCmd (Msg msg) model

    let updateSubs droppedTouches =
        List.fold (handleSub droppedTouches) this.model (subscriptions this.model)

    let processEvent event =
        match event with
        | TimerEnd cmd -> runCmd cmd

    let rec processEvents (events : System.Collections.Concurrent.ConcurrentQueue<Event<Msg>>) =
        state { 
            match events.TryDequeue() with
            | false,_ -> return! getState
            | true,event -> 
                let! model = (match event with TimerEnd cmd -> runCmd cmd)
                do! putState model
                return! processEvents events
            }

    override __.Update(gameTime) =
        let revSprites = List.rev this.sprites
        let touchCol = getTouchState (TouchPanel.GetState())
        let droppedTouches = droppedTouches touchCol this.controller
        this.controller <- updateController touchCol this.controller
        this.model <- handleInput revSprites
        this.model <- updateSubs droppedTouches
        this.model <- lock events (fun () -> execState (processEvents events) this.model)
        base.Update(gameTime)

    //
    // --------- Draw ---------
    //

    override __.Draw gameTime = 
        this.sprites <- view this.model
        this.spriteBatch.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend)
        let findTexture t = Map.find t this.textures
        let scale = float GraphicsAdapter.DefaultAdapter.CurrentDisplayMode.Width / 750.0 // temporary hard-coded scaling
        for sprite in this.sprites do
            for texture in sprite.textures do
                let texture2D = findTexture texture
                let x,y = mapT2 ((*) scale >> int) sprite.position
                let w,h = mapT2 (float >> (*) scale >> int) (texture2D.Width,texture2D.Height)
                this.spriteBatch.Draw(texture2D, Rectangle(x, y, w, h), Color.White * float32 sprite.alpha)
        this.spriteBatch.End()
        base.Draw(gameTime)