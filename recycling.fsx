#load "deciders.fsx" "domain.fsx"
#r "nuget: Suave"
#r "nuget: FSharp.Data"
open FSharp.Data
open System
open Deciders
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Domain

module Serialization =
    open IdCard
    type Api = JsonProvider<"sample.json",SampleIsList = true>
    type ResponseApi = JsonProvider<"response.json", SampleIsList = true>

    let parseRequest (request) =
        let json = Api.Parse(request)
        let command =
            match json.Command.Type with
            | "CalculatePrice" -> CalculatePrice
            | _ -> failwith "Unknown command"
        let events =
            [ for e in json.History  do 
                let event =
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
                CardId e.Payload.CardId, event
            ]
        events, (CardId json.Command.Payload.CardId, command)

    let serializeResponse (CardId cardId,event) =
        match event with
        | PriceWasCalculated e ->
            let json = 
                ResponseApi.Root(
                    createdAt = DateTime.Now,
                    eventId = Guid.NewGuid(),
                    payload = ResponseApi.Payload(cardId = cardId, priceAmount = e.Amount, priceCurrency = e.Currency), ``type`` = "PriceWasCalculated")
            string json
        | _ -> failwith "Event cannot be serialized"

open IdCard              
// web server
let handler (req: HttpRequest)  =
    let events, command =
        req.rawForm
        |> Text.Encoding.UTF8.GetString 
        |> Serialization.parseRequest

    let state = List.fold evolve initialState events
    let newEvents = decide command state
    match List.tryHead newEvents with
    | Some e -> OK (Serialization.serializeResponse e)
    | None -> OK ""


let service = 
   choose [ GET  >=> path "/validate" >=> OK "OK" 
            POST >=> path "/handle-command" >=> request handler
   ]

async {
    startWebServer defaultConfig service
} |> Async.Start
