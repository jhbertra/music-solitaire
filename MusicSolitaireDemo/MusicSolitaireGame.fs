module MusicSolitaireGame

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input.Touch

open Core
open Model
open Update
open View

type MusicSolitaireGame() as this =
    inherit Game()

    let _ = new GraphicsDeviceManager(this)
    let model,initialCmd = initModel(System.Random())

    [<DefaultValue>] val mutable model : Model
    [<DefaultValue>] val mutable sprites : Sprite<Msg> list
    [<DefaultValue>] val mutable textures : Map<string, Texture2D>
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
            |> List.fold (fun m t -> Map.add t (this.Content.Load<Texture2D>(t)) m) (Map[])
        base.LoadContent()

    //
    // --------- Update ---------
    //

    let rec gestures _ =
        if not TouchPanel.IsGestureAvailable then
            []
        else 
            TouchPanel.ReadGesture() :: gestures ()  

    let boundingBox sprite =
        let x,y = sprite.position
        let width,height =
            sprite.textures
            |> List.map (fun t -> Map.find t this.textures)
            |> List.fold (fun (width,height) t -> (max width t.Width),(max height t.Height)) (0, 0)
        (x, y),((float)width + x, (float)height + y)

    let isInSprite (position : Vector2) sprite =
        let (xMin,yMin),(xMax,yMax) = boundingBox sprite
        ((float)position.X) >= xMin
        && ((float)position.X) < xMax
        && ((float)position.Y) >= yMin
        && ((float)position.Y) < yMax

    let rec msgFromGesture getMsg (gesture : GestureSample) sprites =
        match sprites with
        | [] -> None
        | sprite :: tail ->
            if isInSprite gesture.Position sprite then
                match getMsg sprite with
                | Some msg -> Some msg
                | None -> msgFromGesture getMsg gesture tail
            else
                msgFromGesture getMsg gesture tail

    let handleGesture sprites model (gesture : GestureSample) =
        let msg =
            match gesture.GestureType with
            | GestureType.Tap -> msgFromGesture (fun sprite -> sprite.tap) gesture sprites
            | GestureType.FreeDrag -> msgFromGesture (fun sprite -> sprite.touchDown) gesture sprites
            | GestureType.DragComplete -> msgFromGesture (fun sprite -> sprite.touchUp) gesture sprites
            | _ -> None
        match msg with
        | None -> model
        | Some msg -> execCmd (Msg msg) model


    let handleInput model sprites =
        List.fold (handleGesture sprites) model (gestures ())

    override __.Update(gameTime) =
        let revSprites = List.rev this.sprites
        this.model <- handleInput this.model revSprites
        base.Update(gameTime)

    //
    // --------- Draw ---------
    //

    override __.Draw gameTime = 
        this.sprites <- view this.model
        this.spriteBatch.Begin()
        for sprite in this.sprites do
            for texture in sprite.textures do
                let x,y = sprite.position
                this.spriteBatch.Draw(Map.find texture this.textures, Vector2((float32) x, (float32) y), Color.White)
        this.spriteBatch.End()
        base.Draw(gameTime)