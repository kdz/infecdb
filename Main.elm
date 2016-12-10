port module Main exposing (..)

import Html exposing (button, text, div, Html, table, tr, td, node, h2, p, label, input, span, thead, th, tbody, caption)
import Json.Decode exposing (string, float, int)
import Json.Decode as Decode
import Json.Encode as Encode
import String
import Http
import Result
import Menu
import Debug
import Tuple
import Model exposing (..)
import View exposing (..)
import Conversion exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port showMarkers : List Location -> Cmd oneway


init : ( Model, Cmd Msg )
init =
    ( initModel
    , hospitalLoadCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        errorNoOp err =
            let
                _ =
                    Debug.log (toString err)
            in
                ( model, Cmd.none )

        _ =
            ( Debug.log "**** Msg, Mode, Login -> Mode, Login\n\t" ( msg, model.mode, model.loginStatus )
            , Debug.log "\t\t-->" ( (Tuple.first result).mode, (Tuple.first result).loginStatus )
            )

        float s =
            String.toFloat s |> Result.withDefault 0

        mapLocations locatableList =
            locatableList |> List.map (\x -> Location (float x.latitude) (float x.longitude))

        result =
            case msg of
                DB_HospitalTableSucceed incomingHospitals ->
                    ( { model
                        | hospitals = { list = incomingHospitals, sel = Nothing }
                        , fields = tableFields hospitalTable
                        , mapCaption = "Map of Hospitals"
                      }
                    , incomingHospitals |> mapLocations |> showMarkers
                    )

                DB_PatientTableSucceed incomingPatients ->
                    ( { model
                        | patients = { list = incomingPatients, sel = Nothing }
                        , fields = tableFields patientTable
                        , mapCaption = "Map of Patients"
                      }
                    , incomingPatients |> mapLocations |> showMarkers
                    )

                DB_OtherPatientTableSucceed incomingPatients ->
                    ( { model
                        | otherPatients = incomingPatients
                        , fields = tableFields patientTable
                      }
                    , Cmd.none
                    )

                DB_MedicChecksOnSucceed patients ->
                    ( { model | medicChecksOn = patients }, Cmd.none )

                DB_MedicToDoSucceed patients ->
                    ( { model
                        | medicToDo = patients
                        , locationsForMap = mapLocations patients
                        , mapCaption = "Map of Your ToDo List"
                      }
                    , patients |> mapLocations |> showMarkers
                    )

                DB_DiseaseTableSucceed incomingDiseases ->
                    ( { model
                        | diseases = { list = incomingDiseases, sel = Nothing }
                        , fields = tableFields diseaseTable
                      }
                    , Cmd.none
                    )

                DB_MedicTableSucceed incomingMedics ->
                    ( { model
                        | medics = incomingMedics
                        , fields = tableFields medicTable
                      }
                    , Cmd.none
                    )

                DB_SymptomSucceed incomingSymptoms ->
                    ( { model | symptoms = incomingSymptoms }, Cmd.none )

                DB_MedicByMidSucceed medicInfo ->
                    let
                        model_ =
                            { model | mePage = MePageMedic medicInfo }
                    in
                        ( model_, Cmd.none )

                DB_PatientByPidSucceed patientInfo ->
                    let
                        model_ =
                            { model | mePage = MePagePatient patientInfo }
                    in
                        ( model_, Cmd.none )

                DB_RequestFail err ->
                    errorNoOp err

                UI_MenuAct (Menu.Select mode) ->
                    let
                        newMode =
                            Menu.update (Menu.Select mode)
                    in
                        case mode of
                            HospitalPage ->
                                ( { model | mode = newMode }, hospitalLoadCmd )

                            PatientPage ->
                                ( { model | mode = newMode }, patientLoadCmd )

                            DiseasePage ->
                                ( { model | mode = newMode }, diseaseLoadCmd )

                            MyInfoPage ->
                                let
                                    model_ =
                                        { model | mode = newMode }
                                in
                                    ( model_, loadMyPageInfoCmd model )

                UI_UpdateLogin val ->
                    ( { model
                        | loginNumberModel = val
                      }
                    , Cmd.none
                    )

                UI_PIDLoginAttempt ->
                    ( model, validatePidCmd (model.loginNumberModel |> Maybe.withDefault 0 |> toString) )

                UI_MIDLoginAttempt ->
                    ( model, validateMidCmd (model.loginNumberModel |> Maybe.withDefault 0 |> toString) )

                DB_PIDLoginResult pid success ->
                    case success of
                        True ->
                            ( { model
                                | loginStatus =
                                    PatientLoggedIn
                                        ((String.toInt pid)
                                            |> Result.withDefault 0
                                        )
                              }
                            , Cmd.none
                            )

                        False ->
                            ( model, Cmd.none )

                DB_MIDLoginResult mid success ->
                    case success of
                        True ->
                            ( { model
                                | loginStatus =
                                    MedicLoggedIn
                                        ((String.toInt mid)
                                            |> Result.withDefault 0
                                        )
                              }
                            , Cmd.none
                            )

                        False ->
                            ( model, Cmd.none )

                UI_UpdateFieldInput field val ->
                    ( { model | fields = updateField field val model.fields }, Cmd.none )

                UI_DoFieldSearch ->
                    case model.mode of
                        HospitalPage ->
                            ( model, queryHospitalFieldsCmd model )

                        PatientPage ->
                            ( model, queryPatientFieldsCmd model )

                        DiseasePage ->
                            ( model, queryDiseaseFieldsCmd model )

                        _ ->
                            ( model, Cmd.none )

                UI_RowSelected_Ignore x i ->
                    ( model, Cmd.none )

                UI_RowSelected_Hospital tblName ind ->
                    let
                        hospitals =
                            model.hospitals

                        hospWithSelection =
                            { hospitals | sel = Just ind }
                    in
                        ( { model | hospitals = hospWithSelection }
                        , diseaseByHospitalCmd (primaryKey model tblName ind)
                        )

                UI_RowSelected_Disease tblName ind ->
                    -- ( model, sympByDiseaseCmd (primaryKey model tblName ind) )
                    let
                        diseases =
                            model.diseases

                        diseasesWithSelection =
                            { diseases | sel = Just ind }
                    in
                        ( { model | diseases = diseasesWithSelection }
                        , sympByDiseaseCmd (primaryKey model tblName ind)
                        )

                UI_RowSelected_Patient tblName ind ->
                    -- ( model, patientByContactedSourceCmd (primaryKey model tblName ind) )
                    let
                        patients =
                            model.patients

                        patientsWithSelection =
                            { patients | sel = Just ind }
                    in
                        ( { model | patients = patientsWithSelection }
                        , patientByContactedSourceCmd (primaryKey model tblName ind)
                        )
    in
        result


updateField : String -> String -> List Field -> List Field
updateField field val oldFields =
    oldFields
        |> List.map
            (\f ->
                if f.name == field then
                    { f | val = val }
                else
                    f
            )


baseUrl : String
baseUrl =
    "http://kad2185.ngrok.io"



-- "http://kad2185.ngrok.io"
{-
      post_ : Decode.Decoder a -> String -> Http.Body -> Task.Task Http.Error a
      post_ dec url body =
          let
              _ =
                  Debug.log ">>> Ready to POST" ( url, body )
          in
              HttpBuilder.send Http.defaultSettings
                  { verb = "POST"
                  , headers = [ ( "Content-type", "application/json" ) ]
                  , url = url
                  , body = body
                  }
                  |> Http.fromJson dec

   loadTableHelper : Table a -> Decode.Decoder b -> (b -> Msg) -> Cmd Msg
   loadTableHelper tbl dec successMsg =
       let
           body =
               [ ( "table_name", Encode.string tbl.name ), ( "columns", encodeCols tbl.columns ) ]
                   |> Encode.object
                   |> Encode.encode 1
                   |> Http.string
       in
           (post_ dec (baseUrl ++ "/get-table") body)
               |> Task.perform DB_RequestFail successMsg

-}
-- Full table queries


encodeCols : List ( String, a -> String ) -> Encode.Value
encodeCols columns =
    columns
        |> List.map (\( name, getter ) -> Encode.string name)
        |> Encode.list


tblRequestBody : Table a -> Http.Body
tblRequestBody tbl =
    let
        cols : Encode.Value
        cols =
            tbl.columns
                |> List.map (\( name, getter ) -> Encode.string name)
                |> Encode.list
    in
        [ ( "table_name", Encode.string tbl.name ), ( "columns", cols ) ]
            |> Encode.object
            |> Http.jsonBody


hospitalLoadCmd : Cmd Msg
hospitalLoadCmd =
    postWithTwoHandlers "/get-table" (tblRequestBody hospitalTable) (Decode.list hospitalDecoder) DB_HospitalTableSucceed DB_RequestFail



-- loadTableHelper hospitalTable (Decode.list hospitalDecoder) DB_HospitalTableSucceed


patientLoadCmd : Cmd Msg
patientLoadCmd =
    Debug.crash "boom"



-- loadTableHelper patientTable (Decode.list patientDecoder) DB_PatientTableSucceed


diseaseLoadCmd : Cmd Msg
diseaseLoadCmd =
    Debug.crash "boom"



-- loadTableHelper diseaseTable (Decode.list diseaseDecoder) DB_DiseaseTableSucceed
-- Login


validatePidCmd : String -> Cmd Msg
validatePidCmd pid =
    postWithTwoHandlers ("/pid-login" ++ pid)
        Http.emptyBody
        Decode.bool
        (DB_PIDLoginResult pid)
        DB_RequestFail



-- (post_ Decode.bool (baseUrl ++ "/pid-login/" ++ pid) Http.empty)
--     |> Task.perform DB_RequestFail (DB_PIDLoginResult pid)


validateMidCmd : String -> Cmd Msg
validateMidCmd mid =
    postWithTwoHandlers ("/mid-login" ++ mid)
        Http.emptyBody
        Decode.bool
        (DB_MIDLoginResult mid)
        DB_RequestFail



-- (post_ Decode.bool (baseUrl ++ "/mid-login/" ++ mid) Http.empty)
--     |> Task.perform DB_RequestFail (DB_MIDLoginResult mid)
-- Field-specific queries


encodeFields : List Field -> Encode.Value
encodeFields fields =
    fields
        |> List.filter (\f -> not ((String.length f.val) == 0))
        |> List.map (\f -> Encode.list [ Encode.string f.name, Encode.string f.val ])
        |> Encode.list


queryFieldsCmd : Table a -> List Field -> Decode.Decoder b -> (b -> Msg) -> Cmd Msg
queryFieldsCmd tbl fields dec successMsg =
    let
        body =
            [ ( "table_name", Encode.string tbl.name )
            , ( "columns", encodeCols tbl.columns )
            , ( "fields", encodeFields fields )
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/query-fields" body dec successMsg DB_RequestFail


queryHospitalFieldsCmd : Model -> Cmd Msg
queryHospitalFieldsCmd model =
    queryFieldsCmd hospitalTable model.fields (Decode.list hospitalDecoder) DB_HospitalTableSucceed


queryPatientFieldsCmd : Model -> Cmd Msg
queryPatientFieldsCmd model =
    queryFieldsCmd patientTable model.fields (Decode.list patientDecoder) DB_PatientTableSucceed


queryDiseaseFieldsCmd : Model -> Cmd Msg
queryDiseaseFieldsCmd model =
    queryFieldsCmd diseaseTable model.fields (Decode.list diseaseDecoder) DB_DiseaseTableSucceed



-- Join queries


primaryKey : Model -> String -> Int -> String
primaryKey model tblName ind =
    {- Only handles hospital and disease cases -}
    if tblName == "hospital" then
        let
            h =
                (getNth model.hospitals.list ind)
        in
            case h of
                Just hosp ->
                    hosp
                        |> .hospitalName

                Nothing ->
                    ""
    else if tblName == "disease" then
        let
            d =
                (getNth model.diseases.list ind)
        in
            case d of
                Just dis ->
                    dis
                        |> .virus_name

                Nothing ->
                    ""
    else if tblName == "patient" then
        let
            p =
                (getNth model.patients.list ind)
        in
            case p of
                Just pat ->
                    pat |> .pid

                Nothing ->
                    ""
    else
        Debug.crash "Could not find table key"


postWithTwoHandlers : String -> Http.Body -> Decode.Decoder a -> (a -> Msg) -> (Http.Error -> Msg) -> Cmd Msg
postWithTwoHandlers url body decoder succMsg failMsg =
    let
        handler result =
            case result of
                Ok v ->
                    succMsg v

                Err e ->
                    failMsg e
    in
        Http.post (baseUrl ++ url) body decoder |> Http.send handler


sympByDiseaseCmd : String -> Cmd Msg
sympByDiseaseCmd virusName =
    let
        body =
            [ ( "virus_name", Encode.string virusName ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/symptom-by-disease" body (Decode.list symptomDecoder) DB_SymptomSucceed DB_RequestFail


diseaseByHospitalCmd : String -> Cmd Msg
diseaseByHospitalCmd hospName =
    let
        body =
            [ ( "hospital_name", Encode.string hospName )
            , ( "columns", encodeCols diseaseTable.columns )
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/disease-by-hospital" body (Decode.list diseaseDecoder) DB_DiseaseTableSucceed DB_RequestFail


patientByContactedSourceCmd : String -> Cmd Msg
patientByContactedSourceCmd pid =
    let
        body =
            [ ( "pid", Encode.string pid )
            , ( "columns", encodeCols patientTable.columns )
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/patient-by-contacted-source" body (Decode.list patientDecoder) DB_OtherPatientTableSucceed DB_RequestFail



-- Me page load


loadMyPageInfoCmd : Model -> Cmd Msg
loadMyPageInfoCmd model =
    case model.loginStatus of
        Public ->
            Cmd.none

        PatientLoggedIn pid ->
            let
                _ =
                    Debug.log "***Going to batch: patient ByPid; exhibitsCmd, HasCmd, MedicCmd" 0
            in
                Cmd.batch
                    [ patientByPIDCmd pid
                    , patientExhibitsCmd pid
                    , patientHasCmd pid
                    , patientMedicCmd pid
                    ]

        -- patientMePageCmd pid
        MedicLoggedIn mid ->
            Cmd.batch
                [ medicByMIDCmd mid
                , medicChecksOnCmd mid
                , medicsToDoCmd mid
                ]


medicByMIDCmd : Int -> Cmd Msg
medicByMIDCmd mid =
    {- Get a single medic's row from medic -}
    let
        body =
            [ ( "mid", Encode.int mid ), ( "columns", encodeCols medicTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/medic-me" body medicDecoder DB_MedicByMidSucceed DB_RequestFail


medicChecksOnCmd : Int -> Cmd Msg
medicChecksOnCmd mid =
    let
        body =
            [ ( "mid", Encode.int mid ), ( "columns", encodeCols patientTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/medic-checks-on" body (Decode.list patientDecoder) DB_MedicChecksOnSucceed DB_RequestFail


medicsToDoCmd : Int -> Cmd Msg
medicsToDoCmd mid =
    let
        body =
            [ ( "mid", Encode.int mid ), ( "columns", encodeCols patientTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/medic-todo" body (Decode.list patientDecoder) DB_MedicToDoSucceed DB_RequestFail


patientByPIDCmd : Int -> Cmd Msg
patientByPIDCmd pid =
    {- Get a single patient's row from patient -}
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols patientTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/patient-me" body patientDecoder DB_PatientByPidSucceed DB_RequestFail


patientExhibitsCmd : Int -> Cmd Msg
patientExhibitsCmd pid =
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols symptomTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/patient-exhibits" body (Decode.list symptomDecoder) DB_SymptomSucceed DB_RequestFail


patientHasCmd : Int -> Cmd Msg
patientHasCmd pid =
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols diseaseTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/patient-has" body (Decode.list diseaseDecoder) DB_DiseaseTableSucceed DB_RequestFail


patientMedicCmd : Int -> Cmd Msg
patientMedicCmd pid =
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols medicTable.columns ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        postWithTwoHandlers "/patient-medic" body (Decode.list medicDecoder) DB_MedicTableSucceed DB_RequestFail
