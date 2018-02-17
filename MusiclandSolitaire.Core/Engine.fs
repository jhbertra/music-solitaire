module Engine

open Core
open FsGame
open Update
open Draw

let engine (rng : System.Random) = {
    contentManifest = contentManifest
    init = initModel rng
    update = update
    draw = draw
}
