namespace MusicSolitaireDemo

open UIKit
open Foundation

open MusicSolitaireGame
open Model
open Update
open Engine

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    override __.FinishedLaunching (_, _) =
        let game = new Game<Model, Tag>(engine(System.Random()))
        game.Run()
        true