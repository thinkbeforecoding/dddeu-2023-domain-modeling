#load "deciders.fsx"
open System

type CardId = CardId of string

module IdCard =
    type FractionType =
        | ConstructionWaste
        | GreenWaste

    type City = City of string
    type Roof = Roof of string

    type Event =
        | IdCardRegistered of IdCardRegistered
        | IdCardScannedAtEntranceGate of IdCardScannedAtEntranceGate
        | IdCardScannedAtExitGate
        | PriceWasCalculated of PriceWasCalculated
        | FractionWasDropped of FractionWasDropped
    and IdCardRegistered = 
        { Address: string
          City: City
          PersonId: string }
    and IdCardScannedAtEntranceGate =
        { Date: DateTime }
    and PriceWasCalculated =
        { Amount: decimal 
          Currency: string }
    and FractionWasDropped =
        { FractionType: FractionType
          Weight: int }
        

    type Command = 
        | CalculatePrice

    type State = 
        { 
            CardsRoof: Map<CardId, Roof>
            RoofCity: Map<Roof, City>
            RoofExemptions: Map<Roof, Map<FractionType,int>>
            CardsFractions: Map<CardId, Map<FractionType, int>>

        }

    type Price =
        { Price: decimal 
          Exemption: int}

    type CityPrices =
        { ConstructionWaste: Price
          GreenWaste: Price}
        

    let defaultPrices = 
        { ConstructionWaste = { Price = 0.15m; Exemption = 0 }
          GreenWaste = { Price = 0.09m; Exemption = 0  }}
        
    let southParkPrices =
        { ConstructionWaste = { Price = 0.18m; Exemption = 100 }
          GreenWaste = { Price = 0.12m; Exemption = 50 } }

    module City =
        let getPrices (City city) =
            match city with
            | "South Park" -> southParkPrices
            | _ -> defaultPrices

    module Price =
        let apply price roofExemption weight =
            let otherExemption = max 0 (roofExemption - weight)
            let remainingExemption = max 0 (price.Exemption - otherExemption)
            let remaininWeight = max 0 (weight - remainingExemption) 
            decimal remaininWeight * price.Price


    module Fraction =
        let price cityPrices roofExemptions (fractionType, weight) =

            let exemption =
                Map.tryFind fractionType roofExemptions
                |> Option.defaultValue 0
            match fractionType with
            | ConstructionWaste -> Price.apply cityPrices.ConstructionWaste exemption weight 
            | GreenWaste -> Price.apply cityPrices.GreenWaste exemption weight

    module Fractions =
        let empty = Map.empty<FractionType, int>
        let add fractionType weight fractions =
            let currentWeight =
                Map.tryFind fractionType fractions
                |> Option.defaultValue 0
            Map.add fractionType (currentWeight + weight) fractions

    let initialState =  {
        CardsRoof = Map.empty
        RoofCity = Map.empty
        RoofExemptions = Map.empty
        CardsFractions = Map.empty
    }
    let decide (id,cmd) (state: State) = 

        match cmd, state with
        | CalculatePrice, _ ->
            let roof = Map.find id state.CardsRoof 
            let city = Map.find roof state.RoofCity
            let prices = City.getPrices city
            let fractions = 
                Map.tryFind id state.CardsFractions
                |> Option.defaultValue Map.empty
            let exemptions = 
                Map.tryFind roof state.RoofExemptions
                |> Option.defaultValue Map.empty
            let price =
                fractions
                |> Map.toSeq
                |> Seq.sumBy (Fraction.price prices exemptions)

            [ id, PriceWasCalculated { Amount = price; Currency = "EUR"}]
        
        
    let evolve (state: State) (id, event) =
        match event, state with
        | IdCardRegistered e, _ ->
            let roof = Roof e.Address
            { state with 
                RoofCity = Map.add roof e.City state.RoofCity 
                CardsRoof = Map.add id roof state.CardsRoof
            }
        | FractionWasDropped e, _ -> 
            let roof = Map.find id state.CardsRoof
            let exemptions =
                Map.tryFind roof state.RoofExemptions
                |> Option.defaultValue Map.empty
                
            let exemption =
                Map.tryFind e.FractionType exemptions
                |> Option.defaultValue 0
                |> (+) e.Weight

            let newExemptions =
                Map.add e.FractionType exemption exemptions

            let fractions =
                Map.tryFind id state.CardsFractions
                |> Option.defaultValue Fractions.empty


            { state with
                RoofExemptions = Map.add roof newExemptions state.RoofExemptions
                CardsFractions = Map.add id (Fractions.add e.FractionType e.Weight fractions ) state.CardsFractions
            }
        | _ -> state
    let isTerminal state = false

    open Deciders
    let decider =
        { decide = decide
          evolve = evolve
          initialState = initialState
          isTerminal = isTerminal
        }


module Tests =
    open IdCard
    open Deciders
    let (=>) events command = givenWhen decider events command

    let id = CardId "123"
    // no fraction delivered
    [ id,IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id, IdCardScannedAtExitGate ]
    => (id, CalculatePrice)
    =! [ id, PriceWasCalculated { Amount = 0m; Currency = "EUR"} ]

    // a single fraction was delivered
    [ id, IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 71}
      id, IdCardScannedAtExitGate ]
    => (id, CalculatePrice)
    =! [ id, PriceWasCalculated { Amount = 10.65m; Currency = "EUR"} ]

    // multiple fraction delivered
    [ id, IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 51}
      id, FractionWasDropped { FractionType = GreenWaste; Weight = 23}
      id, IdCardScannedAtExitGate ]
    => (id, CalculatePrice)
    =! [ id, PriceWasCalculated { Amount = 9.72m; Currency = "EUR"} ]

    // Construction waste in south park
    [ id, IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 85}
      id, IdCardScannedAtExitGate ]
    // |> List.fold decider.evolve decider.initialState
    => (id, CalculatePrice)
    =! [ id, PriceWasCalculated { Amount = 0m; Currency = "EUR"} ]

    // Construction waste above exemption rules
    [ id, IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 85}
      id, IdCardScannedAtExitGate
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,11) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 63}
      id, IdCardScannedAtExitGate ]
    => (id, CalculatePrice)
    =! [ id, PriceWasCalculated { Amount = 8.64m; Currency = "EUR"} ]

    // Multiple deliveries in South Park
    [ id, IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,03,10) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 60 }
      id, FractionWasDropped { FractionType = GreenWaste; Weight = 30 }
      id, IdCardScannedAtExitGate
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,04,23) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 30 }
      id, FractionWasDropped { FractionType = GreenWaste; Weight = 20 }
      id, IdCardScannedAtExitGate
      id, IdCardScannedAtEntranceGate { Date = DateTime(2023,05,05) }
      id, FractionWasDropped { FractionType = ConstructionWaste; Weight = 20 }
      id, FractionWasDropped { FractionType = GreenWaste; Weight = 10 }
      id, IdCardScannedAtExitGate ]
    => (id, CalculatePrice)
    =! [ id, PriceWasCalculated { Amount = 3.0m; Currency = "EUR"} ] 

    let id587 = CardId "587"
    let id752 = CardId "752"
    let id951 = CardId "951"
    [ id587, IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      id752, IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Sharon Marsh"}
      id951, IdCardRegistered { Address = "635 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Stephen Stotch"}
      id587, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id951, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id587, FractionWasDropped { FractionType = GreenWaste; Weight = 33 }
      id752, IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      id951, FractionWasDropped { FractionType = GreenWaste; Weight = 47 }
      id587, IdCardScannedAtExitGate
      id752, FractionWasDropped { FractionType = GreenWaste; Weight = 42 }
      id951, IdCardScannedAtExitGate
      id752, IdCardScannedAtExitGate ]
    => (id752, CalculatePrice)
    =! [ id752, PriceWasCalculated { Amount = 3.0m; Currency = "EUR"} ] 
