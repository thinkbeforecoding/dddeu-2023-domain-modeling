

type Decider<'c, 'e, 'si, 'so> =
    { decide: 'c -> 'si -> 'e list
      evolve: 'si -> 'e -> 'so
      initialState: 'so
      isTerminal: 'si -> bool
    }

type Decider<'c,'e,'s> = Decider<'c,'e,'s,'s>

let givenWhen decider = 
    fun events command ->
        events
        |> List.fold decider.evolve decider.initialState
        |> decider.decide command

let (=!) actual expected =
    if actual = expected then
        printfn "✅"
    else
        printfn "❌ actual:"
        printfn "%A" actual
        printfn "expected:"
        printfn "%A" expected