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
        let engine = engine(System.Random())
        let game = new TouchGame<Model, Tag>(engine)
        game.Run()
        true