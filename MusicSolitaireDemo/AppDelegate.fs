namespace MusicSolitaireDemo

open System

open UIKit
open Foundation

type Game() as game =
    inherit Microsoft.Xna.Framework.Game() 
    let manager = new Microsoft.Xna.Framework.GraphicsDeviceManager(game)
    override __.Draw _ = game.GraphicsDevice.Clear Microsoft.Xna.Framework.Color.CornflowerBlue

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    override val Window = null with get, set

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        let game = new Game()
        game.Run()
        true
