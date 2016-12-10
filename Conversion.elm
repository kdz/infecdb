module Conversion exposing (..)

import Model exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


-- DECODERS


hospitalDecoder : Decode.Decoder Hospital
hospitalDecoder =
    decode Hospital
        |> required "hospital_name" Decode.string
        |> required "number_of_beds" Decode.string
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string


patientDecoder : Decode.Decoder Patient
patientDecoder =
    decode Patient
        |> required "pid" Decode.string
        |> required "forename" Decode.string
        |> required "surname" Decode.string
        |> required "status" Decode.string
        |> required "phone_number" Decode.string
        |> required "dob" Decode.string
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string


diseaseDecoder : Decode.Decoder Disease
diseaseDecoder =
    decode Disease
        |> required "virus_name" Decode.string
        |> required "incubation" Decode.string
        |> required "duration" Decode.string


symptomDecoder : Decode.Decoder Symptom
symptomDecoder =
    decode Symptom
        |> required "symptom_name" Decode.string
        |> required "description" Decode.string


medicDecoder : Decode.Decoder Medic
medicDecoder =
    decode Medic
        |> required "mid" Decode.string
        |> required "name" Decode.string
        |> required "phone_number" Decode.string
        |> required "hospital_name" Decode.string
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string
