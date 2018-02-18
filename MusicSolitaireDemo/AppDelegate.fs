namespace MusicSolitaireDemo

open UIKit
open Foundation

open FsGame.iOS
open Model
open Engine

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    override __.FinishedLaunching (_, _) =
        let game = new FsGame<Model, Tag>(engine(System.Random()))
        game.Run()
        true