module Engine

open Core
open View
open Model
open Update
open View

let engine (rng : System.Random) = {
    contentManifest = contentManifest
    init = initModel rng
    update = update
    draw = draw
}
