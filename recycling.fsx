#load "deciders.fsx"
#r "nuget: Suave"
#r "nuget: FSharp.Data"
open FSharp.Data
open System
open Deciders
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Http.HttpMethod

type FractionType =
    | ConstructionWaste
    | GreenWaste

type Fraction =
    { FractionType: FractionType
      Weight: int }
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
      Fractions: Fraction list }

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
    let price cityPrices (fraction: Fraction) =
            match fraction.FractionType with
            | ConstructionWaste -> Price.apply cityPrices.ConstructionWaste fraction.Weight 
            | GreenWaste -> Price.apply cityPrices.GreenWaste fraction.Weight

let initialState =  Unregistered
let decide cmd state = 

    match cmd, state with
    | CalculatePrice, Registered state ->
        let prices = City.getPrices state.City
        let price =
            state.Fractions
            |> List.groupBy (fun f -> f.FractionType)
            |> List.map (fun (typ, v) -> { Fraction.FractionType = typ; Weight =  v |> List.sumBy (fun f -> f.Weight) } )
            |> List.sumBy (Fraction.price prices)

        [ PriceWasCalculated { Amount = price; Currency = "EUR"}]
    | _ -> []
    
    
let evolve state event =
    match event, state with
    | IdCardRegistered e, Unregistered -> Registered { City =e.City; Fractions = []} 
    | FractionWasDropped e, Registered state -> Registered { state with Fractions = { FractionType = e.FractionType; Weight = e.Weight} :: state.Fractions}
    | _ -> state
let isTerminal state = true


let decider =
    { decide = decide
      evolve = evolve
      initialState = initialState
      isTerminal = isTerminal
      }

let (=>) events command = givenWhen decider events command


type Api = JsonProvider<"sample.json",SampleIsList = true>
type ResponseApi = JsonProvider<"response.json", SampleIsList = true>

let parseRequest (request) =
    let json = Api.Parse(request)
    let command =
        match json.Command.Type with
        | "CalculatePrice" ->  CalculatePrice
        | _ -> failwith "Unknown command"
    let events =
        [ for e in json.History  do 
            match e.Type with
            | "IdCardRegistered" -> 
                IdCardRegistered
                    { Address = e.Payload.Address.Value
                      City = City (e.Payload.City.Value)
                      PersonId = e.Payload.PersonId.Value
                                    }
            | "IdCardScannedAtEntranceGate" ->
                IdCardScannedAtEntranceGate 
                    { Date = e.Payload.Date.Value }
            | "IdCardScannedAtExitGate" ->
                IdCardScannedAtExitGate
            | "FractionWasDropped" ->
                FractionWasDropped {
                    FractionType =
                        match e.Payload.FractionType.Value with
                        | "Construction waste" -> ConstructionWaste
                        | "Green waste" -> GreenWaste
                        | _ -> failwith "Unknown fraction type"
                    Weight = e.Payload.Weight.Value
                }
            | _ -> failwith "Unknown event"
        ]
    json.Command.Payload.CardId,  events, command

let serialize cardId event =
    match event with
    | PriceWasCalculated e ->
        let json = 
            ResponseApi.Root(
                createdAt = DateTime.Now,
                eventId = Guid.NewGuid(),
                payload = ResponseApi.Payload(cardId = cardId, priceAmount = e.Amount, priceCurrency = e.Currency), ``type`` = "PriceWasCalculated")
        string json
    | _ -> failwith "Event cannot be serialized"
            














let handler (req: HttpRequest)  =
    let cardId, events, command =
        req.rawForm
        |> Text.Encoding.UTF8.GetString 
        |> parseRequest

    let state = List.fold evolve initialState events
    let newEvents = decide command state
    match List.tryHead newEvents with
    | Some e -> OK (serialize cardId e)
    | None -> OK ""

[ IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
  IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
  IdCardScannedAtExitGate ]
=> CalculatePrice
=! [ PriceWasCalculated { Amount = 0m; Currency = "EUR"} ]




[ IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
  IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
  FractionWasDropped { FractionType = ConstructionWaste; Weight = 71}
  IdCardScannedAtExitGate ]
=> CalculatePrice
=! [ PriceWasCalculated { Amount = 10.65m; Currency = "EUR"} ]

[ IdCardRegistered { Address = "Point Dume"; City = City "Malibu"; PersonId = "Tony Stark"}
  IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
  FractionWasDropped { FractionType = ConstructionWaste; Weight = 51}
  FractionWasDropped { FractionType = GreenWaste; Weight = 23}
  IdCardScannedAtExitGate ]
=> CalculatePrice
=! [ PriceWasCalculated { Amount = 9.72m; Currency = "EUR"} ]

[ IdCardRegistered { Address = "260 Avenue de Los Mexicanos"; City = City "South Park"; PersonId = "Randy Marsh"}
  IdCardScannedAtEntranceGate { Date = DateTime(2023,02,10) }
  FractionWasDropped { FractionType = ConstructionWaste; Weight = 85}
  IdCardScannedAtExitGate ]
=> CalculatePrice
=! [ PriceWasCalculated { Amount = 0m; Currency = "EUR"} ]

let mutable mutHandler = handler
mutHandler <- handler




let service = 
   choose [ GET  >=> path "/validate" >=> OK "OK" 
            POST >=> path "/handle-command" >=> request mutHandler
   ]

async {
    startWebServer defaultConfig service
} |> Async.Start
