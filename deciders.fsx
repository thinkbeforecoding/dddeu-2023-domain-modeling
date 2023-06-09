

type Decider<'c, 'e, 'si, 'so> =
    { decide: 'c -> 'si -> 'e list
      evolve: 'si -> 'e -> 'so
      initialState: 'so
      isTerminal: 'si -> bool
    }

type Decider<'c,'e,'s> = Decider<'c,'e,'s,'s>


// takes a decider, and create a decider that can manage many instances
// indexed by a string
let many (decider: Decider<'c,'e,'s>) : Decider<'a * 'c, 'a * 'e, Map<'a,'s>> =
    { decide =
        fun (id, cmd) states ->
            let state =
                match Map.tryFind id states with
                | Some s -> s
                | None -> decider.initialState
            let events = decider.decide cmd state
            events |> List.map (fun e -> id, e)

      evolve =
        fun states (id, event) ->
            let state =
                match Map.tryFind id states with
                | Some s -> s
                | None -> decider.initialState
            let newState =decider.evolve state event
            Map.add id newState states

      initialState = Map.empty

      isTerminal =
        fun states ->
            Map.forall (fun _ s -> decider.isTerminal s) states
    }

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