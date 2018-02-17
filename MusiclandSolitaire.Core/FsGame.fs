module FsGame

open System

//
// --------- Types ---------
//

type ContentManifest = {
    root : string
    textures : string list
    sfx: string list
}


type SoundMode =
    | Overlap
    | NoOverlap


type GameTime = {
    elapsed : TimeSpan
    total : TimeSpan
    isRunningSlowly : bool
}


type Point = Point of float*float


type Delta = Delta of float*float


type Touch = Touch of int * Point


type BoundingBox = BoundingBox of float*float*float*float


type GameObject<'a> = {
    textures : string list
    box : BoundingBox
    alpha : float
    tag : 'a
}


type GameObjectDrawRequest<'a> =
    | Area of string * Point * 'a
    | Sprite of string list * Point * float * 'a


type GameState<'m, 't> = {
    touches : Touch list
    gameTime : GameTime
    objects : GameObject<'t> list
    model : 'm
}


type Cmd<'m, 't> =
    | PlaySound of string * SoundMode * float
    | Delay of float * Update<'m, 't>


and UpdateResult<'m, 't> = UpdateResult of 'm * Cmd<'m, 't> list


and Update<'m, 't> = GameState<'m, 't> -> UpdateResult<'m, 't>


type Draw<'m, 't> = 'm -> GameObjectDrawRequest<'t> list


type GameEngine<'m, 't> = {
    contentManifest : ContentManifest
    init : 'm
    update : Update<'m, 't>
    draw : Draw<'m, 't>
}



//
// --------- Functions ---------
//

let area ( BoundingBox ( _, _, w, h ) ) = w * h


let bounds { box = BoundingBox (x,y,w,h) } = x,y,w,h


let distance (Point (x1,y1)) (Point (x2,y2)) = sqrt ((x2 - x1)**2.0 + (y2 - y1)**2.0)


let intersect (BoundingBox (xa,ya,wa,ha)) (BoundingBox (xb,yb,wb,hb)) =
    let x1a = xa + wa
    let x1b = xb + wb
    let y1a = ya + ha
    let y1b = yb + hb
    let xc = max xa xb
    let yc = max ya yb
    let x1c = min x1a x1b
    let y1c = min y1a y1b
    let wc = x1c - xc
    let hc = y1c - yc
    if wc < 0.0 || hc < 0.0 then
        None
    else
        Some (BoundingBox (xc, yc, wc, hc))


let lerp ( Point ( x1, y1 ) ) ( Point ( x2, y2 ) ) t = Point ( (x2 - x1) * t + x1, (y2 - y1) * t + y1 )


let subtract (Point (x1,y1)) (Point (x2,y2)) = Delta (x2 - x1, y2 - y1)


let union (BoundingBox (xa,ya,wa,ha)) (BoundingBox (xb,yb,wb,hb)) = 
    let x1a = xa + wa
    let x1b = xb + wb
    let y1a = ya + ha
    let y1b = yb + hb
    let xc = min xa xb
    let yc = min ya yb
    let x1c = max x1a x1b
    let y1c = max y1a y1b
    let wc = x1c - xc
    let hc = y1c - yc
    BoundingBox (xc, yc, wc, hc)