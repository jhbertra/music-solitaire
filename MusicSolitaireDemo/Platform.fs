module Platform

open Core

//
// --------- Cmd --------
//

let rec runCmd updateFn cmd = 
    state {
        let! model = getState
        match cmd with
        | Term -> return model
        | Msg msg -> 
            let newModel,nextCmd = updateFn msg model
            do! putState newModel
            return! runCmd updateFn nextCmd
        }

let execGameState updateFn cmd model =
    let gameStateBuilder = runCmd updateFn cmd
    execState gameStateBuilder model



//
// --------- Game --------
//

type Game() as game =
    inherit Microsoft.Xna.Framework.Game() 
    let manager = new Microsoft.Xna.Framework.GraphicsDeviceManager(game)
    override __.Draw _ = game.GraphicsDevice.Clear Microsoft.Xna.Framework.Color.CornflowerBlue