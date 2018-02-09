namespace MusicSolitaireDemo

open UIKit
open Foundation

open MusicSolitaireGame
open Engine

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    override val Window = null with get, set

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        let game = new Game(engine(new System.Random()))
        game.Run()
        true