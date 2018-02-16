module Touch

open Core

type GestureSample = GestureSample of GameTime * Touch

let gestureSample gameTime touch  = GestureSample (gameTime, touch)

type TouchEventType =
    | TouchDown of Point
    | TouchMoved of Point
    | TouchUp of Point

type TouchEvent = int * GameTime * TouchEventType

let touchEvent gameTime = function
    | (Some (Touch _)),(Some (Touch (id,pos))) ->
        [id,gameTime,TouchMoved pos]

    | (Some (Touch (id,pos))),None ->
        [id,gameTime,TouchUp pos]

    | None,(Some (Touch (id,pos))) ->
        [id,gameTime,TouchDown pos]

    | _ -> []

let touchEvents previousTouches newTouches gameTime =
    fullJoin previousTouches newTouches id id 
    |> List.collect (touchEvent gameTime)

type PendingGestureType =
    | Tap of GameTime * Point
    | Drag of Point

type PendingGesture = int * PendingGestureType

type Delta = Delta of float*float

type GestureType =
    | TouchDown of Point
    | TouchUp of Point
    | Tap of Point
    | Drag of Point * Delta

type Gesture = int * GestureType

type EventProcessResult =
    | PendingGesture of PendingGesture
    | Gesture of Gesture

let distance (Point (x1,y1)) (Point (x2,y2)) = sqrt ((x2 - x1)**2.0 + (y2 - y1)**2.0)

let subtract (Point (x1,y1)) (Point (x2,y2)) = Delta (x2 - x1, y2 - y1)

let dragThreshold = 5.0

let processEvent = function
    | _,(Some (id,gameTime,TouchEventType.TouchDown position)) -> 
            [
            Gesture(id,TouchDown position)
            PendingGesture(id,PendingGestureType.Tap (gameTime,position))
            ] 

    | (Some (_,(PendingGestureType.Tap (tapStartTime, startPos)))),(Some (id,time,TouchEventType.TouchUp endPos)) 
        when time.total.Subtract(tapStartTime.total).Milliseconds < 500 && distance startPos endPos < dragThreshold ->
            [
            Gesture(id,TouchUp endPos)
            Gesture(id,Tap endPos)
            ]

    | (Some (_,(PendingGestureType.Tap (tapStartTime, startPos)))),(Some (id,time,TouchMoved endPos)) 
        when time.total.Subtract(tapStartTime.total).Milliseconds < 500 && distance startPos endPos < dragThreshold ->
            [PendingGesture(id,PendingGestureType.Tap (tapStartTime,startPos))]

    | (Some (_,(PendingGestureType.Tap (tapStartTime, startPos)))),(Some (id,time,TouchMoved endPos)) 
        when time.total.Subtract(tapStartTime.total).Milliseconds >= 500 && distance startPos endPos < dragThreshold ->
            [PendingGesture(id,PendingGestureType.Drag (startPos))]

    | (Some (_,(PendingGestureType.Tap (_, startPos)))),(Some (id,_,TouchEventType.TouchUp endPos)) 
        when distance startPos endPos >= dragThreshold ->
            [
            Gesture(id,TouchUp endPos)
            Gesture(id,Drag (startPos,(subtract startPos endPos)))
            ]

    | (Some (_,(PendingGestureType.Tap (_, startPos)))),(Some (id,_,TouchMoved endPos)) 
        when distance startPos endPos >= dragThreshold ->
            [
            Gesture(id,Drag (startPos,(subtract startPos endPos)))
            PendingGesture(id, PendingGestureType.Drag (endPos))
            ]

    | (Some (_,(PendingGestureType.Drag startPos))),(Some (id,_,TouchMoved endPos)) ->
        [
        Gesture(id,Drag (startPos,(subtract startPos endPos)))
        PendingGesture(id, PendingGestureType.Drag (endPos))
        ]

    | (Some (_,(PendingGestureType.Drag startPos))),(Some (id,_,TouchEventType.TouchUp endPos)) ->
        [
        Gesture(id,Drag (startPos,(subtract startPos endPos)))
        Gesture(id,TouchUp endPos)
        PendingGesture(id, PendingGestureType.Drag (endPos))
        ]

    | _,(Some (id,_,TouchEventType.TouchUp pos)) -> [Gesture(id,TouchUp pos)]

    | _ -> [] 

let processEvents (previousPending : PendingGesture list) (events : TouchEvent list) = 
    fullJoin previousPending events fst (fun (id,_,_) -> id) 
    |> List.collect processEvent

let gestures = List.collect (function Gesture x -> [x] | _ -> [])

let pendingGestures = List.collect (function PendingGesture x -> [x] | _ -> [])