#load "deciders.fsx"
open System

type FractionType =
    | ConstructionWaste
    | GreenWaste

type City = City of string

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
    | Unregistered
    | Registered of Registered
and Registered =
    { City: City
      Fractions: Map<FractionType,int> }

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
    let apply price weight =
        let remaininWeight = max 0 (weight - price.Exemption) 
        decimal remaininWeight * price.Price


module Fraction =
    let price cityPrices (fractionType, weight) =
            match fractionType with
            | ConstructionWaste -> Price.apply cityPrices.ConstructionWaste weight 
            | GreenWaste -> Price.apply cityPrices.GreenWaste weight

module Fractions =
    let empty = Map.empty<FractionType, int>
    let add fractionType weight fractions =
        let currentWeight =
            Map.tryFind fractionType fractions
            |> Option.defaultValue 0
        Map.add fractionType (currentWeight + weight) fractions

let initialState =  Unregistered
let decide cmd state = 

    match cmd, state with
    | CalculatePrice, Registered state ->
        let prices = City.getPrices state.City
        let price =
            state.Fractions
            |> Map.toSeq
            |> Seq.sumBy (Fraction.price prices)

        [ PriceWasCalculated { Amount = price; Currency = "EUR"}]
    | _ -> []
    
    
let evolve state event =
    match event, state with
    | IdCardRegistered e, Unregistered -> Registered { City =e.City; Fractions = Map.empty} 
    | FractionWasDropped e, Registered state -> Registered { state with Fractions = Fractions.add e.FractionType e.Weight state.Fractions } 
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
    let (=>) events command = givenWhen decider events command

    // no fraction delivered
    [ IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
      IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      IdCardScannedAtExitGate ]
    => CalculatePrice
    =! [ PriceWasCalculated { Amount = 0m; Currency = "EUR"} ]

    // a single fraction was delivered
    [ IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
      IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 71}
      IdCardScannedAtExitGate ]
    => CalculatePrice
    =! [ PriceWasCalculated { Amount = 10.65m; Currency = "EUR"} ]

    // multiple fraction delivered
    [ IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
      IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 51}
      FractionWasDropped { FractionType = GreenWaste; Weight = 23}
      IdCardScannedAtExitGate ]
    => CalculatePrice
    =! [ PriceWasCalculated { Amount = 9.72m; Currency = "EUR"} ]

    // Construction waste in south park
    [ IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 85}
      IdCardScannedAtExitGate ]
    => CalculatePrice
    =! [ PriceWasCalculated { Amount = 0m; Currency = "EUR"} ]

    // Construction waste above exemption rules
    [ IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 85}
      IdCardScannedAtExitGate
      IdCardScannedAtEntranceGate { Date = DateTime(2023,02,11) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 63}
      IdCardScannedAtExitGate ]
    => CalculatePrice
    =! [ PriceWasCalculated { Amount = 8.64m; Currency = "EUR"} ]

    // Multiple deliveries in South Park
    [ IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
      IdCardScannedAtEntranceGate { Date = DateTime(2023,03,10) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 60 }
      FractionWasDropped { FractionType = GreenWaste; Weight = 30 }
      IdCardScannedAtExitGate
      IdCardScannedAtEntranceGate { Date = DateTime(2023,04,23) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 30 }
      FractionWasDropped { FractionType = GreenWaste; Weight = 20 }
      IdCardScannedAtExitGate
      IdCardScannedAtEntranceGate { Date = DateTime(2023,05,05) }
      FractionWasDropped { FractionType = ConstructionWaste; Weight = 20 }
      FractionWasDropped { FractionType = GreenWaste; Weight = 10 }
      IdCardScannedAtExitGate ]
    => CalculatePrice
    =! [ PriceWasCalculated { Amount = 3.0m; Currency = "EUR"} ] 