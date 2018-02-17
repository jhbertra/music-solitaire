module State

open Core


type State<'s,'v> = State of ('s -> 'v * 's)


let inline create s = State s


let inline fromValue x = create ( fun s -> x , s ) 


let inline run initial ( State f ) = f initial


let inline eval initial = run initial >> fst


let inline exec initial = run initial >> snd


let inline bind f x = 
    create ( fun s -> 
        let result , s = run s x
        f result |> run s )


let inline map f = f >> fromValue |> bind


let get = State (fun s -> (s,s))


let inline put newState = State (fun s -> ((), newState))


let inline modify f = get |> bind (f >> put)


let inline gets f = get |> map f


let inline (<+>) s1 s2 = 
    create ( fun s ->
        let result , s = run s s1
        run s s2 )


let inline (<*>) sf sa =  sf |> ( (flip map) sa |> bind )


let inline (<!>) f sa = (fromValue f) <*> sa


let inline (<?>) sf a = sf <*> (fromValue a)


type StateBuilder() =

    member inline this.Zero () = create ( fun s -> (), s )

    member inline this.Return x = fromValue x

    member inline this.ReturnFrom x = x

    member inline this.Bind ( x , f ) = bind f x

    member inline this.Combine (x1, x2) = x1 <+> x2

    member inline this.Delay f = f ()

    member inline this.For (seq, f) =
        if Seq.length seq = 0 then
            this.Zero ()
        else
            seq
            |> Seq.map f
            |> Seq.reduceBack (<+>)

    member this.While (f, x) =
        if f () then
            x <+> this.While (f, x)
        else
            this.Zero ()


let builder = StateBuilder()