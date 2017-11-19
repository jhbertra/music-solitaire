﻿namespace MusicSolitaireDemo

open UIKit
open Foundation

open MusicSolitaireGame

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    override val Window = null with get, set

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        let game = new MusicSolitaireGame()
        game.Run()
        true