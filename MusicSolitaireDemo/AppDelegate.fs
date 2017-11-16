namespace MusicSolitaireDemo

open UIKit
open Foundation

open MusicSolitaire

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    override val Window = null with get, set

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        let model,cmd = initModel(new System.Random())
        let game = new MusicSolitaireGame(model, cmd)
        game.Run()
        true