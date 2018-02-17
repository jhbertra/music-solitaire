﻿module MusicSolitaireGame

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Input.Touch

open Core

type Event<'m, 't> = TimerEnd of Update<'m, 't>

let vector2ToFloatTuple (vec : Microsoft.Xna.Framework.Vector2) = ((float)vec.X,(float)vec.Y)
    
let scale = 750.0 / float GraphicsAdapter.DefaultAdapter.CurrentDisplayMode.Width // temporary hard-coded scaling

let getTouches (touchCol: TouchCollection) =
    touchCol
    |> List.ofSeq
    |> List.map (fun touch -> 
        Touch (touch.Id,touch.Position |> vector2ToFloatTuple |> mapT2 ((flip (/)) scale) |> Point ))

type Game<'m, 't>(engine : GameEngine<'m, 't>) as this =
    inherit Microsoft.Xna.Framework.Game()

    let _ = new Microsoft.Xna.Framework.GraphicsDeviceManager(this)
    let events = System.Collections.Concurrent.ConcurrentQueue<Event<'m, 't>>()

    [<DefaultValue>] val mutable model : 'm
    [<DefaultValue>] val mutable objects : GameObject<'t> list
    [<DefaultValue>] val mutable textures : Map<string, Texture2D>
    [<DefaultValue>] val mutable sfxInstances : Map<string, SoundEffectInstance>
    [<DefaultValue>] val mutable sfx : Map<string, SoundEffect>
    [<DefaultValue>] val mutable spriteBatch : SpriteBatch

    let rec runCommands = function
        | [] -> ()
        | PlaySound (sound, mode, volume)::xs ->
            optional {
                let! sfx = this.sfxInstances.TryFind sound
                if sfx.State = SoundState.Stopped then
                    sfx.Volume <- float32 volume
                    sfx.Play() |> ignore
                else
                    match mode with
                    | Overlap -> 
                            let! sfx = this.sfx.TryFind sound
                            let instance = sfx.CreateInstance()
                            instance.Volume <- float32 volume
                            instance.Play() |> ignore
                    | NoOverlap -> () |> ignore
            } |> ignore
            runCommands xs
        | Delay (time, update)::xs ->
            let timer = new System.Timers.Timer(1000.0 * time)
            timer.AutoReset <- false
            timer.Enabled <- true
            timer.Elapsed.Add(fun _ -> events.Enqueue(TimerEnd update) |> ignore)
            timer.Start()
            runCommands xs

    //
    // --------- Initialize ---------
    //

    override this.Initialize() = 
        this.spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        this.model <- engine.init
        TouchPanel.EnabledGestures <- GestureType.Tap ||| GestureType.FreeDrag ||| GestureType.DragComplete
        base.Initialize()

    //
    // --------- Load ---------
    //

    let findTexture t = Map.find t this.textures

    let rec makeBox (findTexture : string -> Texture2D) (Point (x,y)) = function
        | [] -> BoundingBox (x, y, 0.0, 0.0)
        | t::ts ->
            let texture = findTexture t
            let tBox = BoundingBox (x, y, float texture.Width, float texture.Height)
            makeBox findTexture (Point (x,y)) ts |> union tBox

    let toGameObject findTexture = function
        | Area (texture, point, tag) -> { textures = []; box = makeBox findTexture point [texture]; alpha = 1.0; tag = tag }
        | Sprite (textures, point, alpha, tag) -> { textures = textures; box = makeBox findTexture point textures; alpha = alpha; tag = tag }
        

    override this.LoadContent() =
        let manifest = engine.contentManifest
        this.Content.RootDirectory <- manifest.root
        this.textures <- 
            manifest.textures
            |> List.fold (fun m t -> Map.add t (this.Content.Load<Texture2D>("textures/" + t)) m) (Map[])
        let sfxList = manifest.sfx |> List.map (fun s -> (s,this.Content.Load<SoundEffect>("sfx/" + s)))
        this.sfxInstances <- 
            sfxList
            |> List.fold (fun m (s,fx) -> Map.add s (fx.CreateInstance()) m) (Map[])
        this.sfx <- 
            sfxList
            |> List.fold (fun m (s,fx) -> Map.add s (fx) m) (Map[])
        this.objects <- engine.draw this.model |> List.map (toGameObject findTexture)
        base.LoadContent()

    //
    // --------- Update ---------
    //

    let gameSpace = (*) scale
    
    let processEvent (TimerEnd callback) = callback

    let rec processEvents (events : System.Collections.Concurrent.ConcurrentQueue<Event<'m, 't>>) gameState =
        state {
            match events.TryDequeue() with
            | false,_ -> return gameState
            | true,event ->
                let! commands = getState
                let (UpdateResult (model,newCommands)) = processEvent event gameState
                do! putState (commands @ newCommands)
                return! processEvents events { gameState with model = model } 
        }

    override this.Update(gameTime) =
        let touches = getTouches (TouchPanel.GetState())
        let gameState = { 
            touches = touches
            gameTime = 
                { 
                elapsed = gameTime.ElapsedGameTime
                total = gameTime.TotalGameTime
                isRunningSlowly = gameTime.IsRunningSlowly 
                }
            objects = this.objects
            model = this.model 
            }
        let model,commands = 
            state {
                let! gameState = lock events (fun () -> processEvents events gameState)
                let! commands = getState
                let (UpdateResult (model,newCommands)) = engine.update gameState
                do! putState (commands @ newCommands)
                return model
            }
            |> runState []
        this.model <- model
        runCommands commands                
        base.Update(gameTime)

    //
    // --------- Draw ---------
    //

    override this.Draw gameTime = 
        this.objects <- engine.draw this.model |> List.map (toGameObject findTexture)
        this.spriteBatch.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend)
        for object in this.objects do
            for texture in object.textures do
                let texture2D = findTexture texture
                let x,y,w,h = bounds object |> mapT4 gameSpace |> mapT4 int
                this.spriteBatch.Draw(texture2D, Microsoft.Xna.Framework.Rectangle(x, y, w, h), Microsoft.Xna.Framework.Color.White * float32 object.alpha)
        this.spriteBatch.End()
        base.Draw(gameTime)