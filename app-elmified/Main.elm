port module Main exposing (..)

import Html exposing (button, text, div, Html, table, tr, td, node, h2, p, label, input, span, thead, th, tbody, caption)
import Html.Attributes exposing (style, href, rel, type', class, id, for, src, attribute)
import Html.Events exposing (onClick, onInput)
import Html.App exposing (program)
import Http
import Json.Decode exposing (string, float, int)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Input.Number as Number
import String
import Result
import Menu
import Array
import Debug
import Task exposing (perform)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port showMarkers : List Location -> Cmd oneway



-- MODEL


type alias Location =
    { lat : Float, lng : Float }


type alias SelList a =
    { list : List a, sel : Maybe Int }


type alias Model =
    { hospitals : SelList Hospital
    , patients : SelList Patient
    , otherPatients : List Patient
    , medicChecksOn : List Patient
    , medicToDo : List Patient
    , locationsForMap : List Location
    , mapCaption : String
    , diseases : SelList Disease
    , symptoms : List Symptom
    , medics : List Medic
    , mode : Mode
    , loginStatus : Status
    , mePage : MePage
    , loginNumberModel : Number.Model
    , loginNumberOptions : Number.Options
    , fields : List Field
    }


initModel : Model
initModel =
    { hospitals = { list = [], sel = Nothing }
    , patients = { list = [], sel = Nothing }
    , otherPatients = []
    , medicChecksOn = []
    , medicToDo = []
    , locationsForMap = []
    , mapCaption = ""
    , diseases = { list = [], sel = Nothing }
    , symptoms = []
    , medics = []
    , mode = HospitalPage
    , loginStatus = Public
    , mePage = NoMePage
    , loginNumberModel = (Number.init)
    , loginNumberOptions =
        ({ id = "NumberInput"
         , maxLength = Just 7
         , maxValue = Nothing
         , minValue = Nothing
         }
        )
    , fields = tableFields hospitalTable
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , hospitalLoadCmd
    )


type Mode
    = HospitalPage
    | DiseasePage
      -- (TODO) contact tracing
    | PatientPage
    | MyInfoPage


type Status
    = Public
    | PatientLoggedIn Int
    | MedicLoggedIn Int


type MePage
    = MePageMedic Medic
    | MePagePatient Patient
    | NoMePage


type alias Field =
    { name : String
    , val : String
    }


tableFields : Table a -> List Field
tableFields tbl =
    tbl.columns
        |> List.map (\( name, getter ) -> Field name "")


menu : List ( String, Mode )
menu =
    [ ( "Hospitals", HospitalPage )
    , ( "Diseases", DiseasePage )
    , ( "Patients", PatientPage )
    , ( "Me", MyInfoPage )
    ]


type alias Table a =
    -- Table Name + Column Definitions (postgres column name + Elm getter-function)
    -- Can support generic view, filter, and back-end GET / POST calls
    -- e.g. see viewTable, which makes html for any table
    -- One of these can be made part of model for each tab
    -- Can add SelectableRows to select 0-or-1 row for join-based queries
    { name : String, columns : List ( String, a -> String ) }


hospitalTable : Table Hospital
hospitalTable =
    { name = "hospital"
    , columns =
        [ ( "hospital_name", .hospitalName )
        , ( "number_of_beds", .numberOfBeds )
        , ( "latitude", .latitude )
        , ( "longitude", .longitude )
        ]
    }


patientTable : Table Patient
patientTable =
    { name = "patient"
    , columns =
        [ ( "pid", .pid )
        , ( "forename", .forename )
        , ( "surname", .surname )
        , ( "status", .status )
        , ( "phone_number", .phoneNumber )
        , ( "dob", .dob )
        , ( "latitude", .latitude )
          -- TODO: add search type info to correctly search non-strings
        , ( "longitude", .longitude )
        ]
    }


diseaseTable : Table Disease
diseaseTable =
    { name = "disease"
    , columns =
        [ ( "virus_name", .virus_name )
        , ( "incubation", .incubation )
        , ( "duration", .duration )
        ]
    }


symptomTable : Table Symptom
symptomTable =
    { name = "symptom"
    , columns =
        [ ( "symptom_name", .symptom_name )
        , ( "description", .description )
        ]
    }


medicTable : Table Medic
medicTable =
    { name = "medic"
    , columns =
        [ ( "mid", .mid )
        , ( "name", .name )
        , ( "hospital_name", .hospitalName )
        , ( "phone_number", .phoneNumber )
        , ( "latitude", .latitude )
        , ( "longitude", .longitude )
        ]
    }



-- MAIN PROBLEM-DOMAIN OBJECT TYPES


type alias Hospital =
    { hospitalName : String
    , numberOfBeds : String
    , latitude : String
    , longitude : String
    }


type alias Patient =
    { pid : String
    , forename : String
    , surname : String
    , status : String
    , phoneNumber : String
    , dob : String
    , latitude : String
    , longitude : String
    }


type alias Disease =
    { virus_name : String
    , incubation : String
    , duration : String
    }


type alias Symptom =
    { symptom_name : String
    , description : String
    }


type alias Medic =
    { mid : String
    , name : String
    , phoneNumber : String
    , hospitalName : String
    , latitude : String
    , longitude : String
    }


type Msg
    = DB_HospitalTableSucceed (List Hospital)
    | DB_RequestFail Http.Error
    | UI_MenuAct (Menu.Msg Mode)
    | UI_UpdateLogin Number.Msg
    | UI_PIDLoginAttempt
    | UI_MIDLoginAttempt
    | DB_PIDLoginSuccess String Bool
    | DB_MIDLoginSuccess String Bool
    | DB_PatientTableSucceed (List Patient)
    | DB_OtherPatientTableSucceed (List Patient)
    | DB_MedicToDoSucceed (List Patient)
    | DB_MedicChecksOnSucceed (List Patient)
      -- ----------
    | DB_DiseaseTableSucceed (List Disease)
      -- ----------
    | DB_SymptomSucceed (List Symptom)
    | DB_MedicTableSucceed (List Medic)
    | DB_MedicByMidSucceed Medic
    | DB_PatientByPidSucceed Patient
    | UI_UpdateFieldInput String String
    | UI_DoFieldSearch
    | UI_RowSelected_Ignore String Int
    | UI_RowSelected_Hospital String Int
    | UI_RowSelected_Disease String Int
    | UI_RowSelected_Patient String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            ( Debug.log "**** Msg, Mode, Login -> Mode, Login\n\t" ( msg, model.mode, model.loginStatus )
            , Debug.log "\t\t-->" ( (fst result).mode, (fst result).loginStatus )
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
                        model' =
                            { model | mePage = MePageMedic medicInfo }
                    in
                        ( model', Cmd.none )

                DB_PatientByPidSucceed patientInfo ->
                    let
                        model' =
                            { model | mePage = MePagePatient patientInfo }
                    in
                        ( model', Cmd.none )

                DB_RequestFail err ->
                    ( model, Cmd.none )

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
                                    model' =
                                        { model | mode = newMode }
                                in
                                    ( model', loadMyPageInfoCmd model )

                UI_UpdateLogin numberMsg ->
                    ( { model
                        | loginNumberModel = Number.update numberMsg model.loginNumberModel
                      }
                    , Cmd.none
                    )

                UI_PIDLoginAttempt ->
                    ( model, validatePidCmd model.loginNumberModel.value )

                UI_MIDLoginAttempt ->
                    ( model, validateMidCmd model.loginNumberModel.value )

                DB_PIDLoginSuccess pid success ->
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

                DB_MIDLoginSuccess mid success ->
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
    "http://localhost:7777"



-- POST REQUESTS
{-
   Functionality

     1) Login by mid or pid.

     2) View full tables: Hospital, Disease, Patient (restricted)

     3) Search each table by specified field values.

     4) Join-searches:
         - Hospital -> diseases a hospital offers treatment for,
                       medics located at a hospital
         - Patient -> patients that exhibit or have a disease
         - Disease -> symptoms produced by a disease

     5) Me-page (Patient)
         - patient data (updates)
         - has and exhibits, if any
         - hospitals admitted to, if any
         - medic checked on by
         - list of who they've contacted (can be empty) (insert/delete)

     6) Me-page (Medic)
         - medic data (updates)
         - todo list of five closest patients
         - patients checked on

-}


post' : Decode.Decoder a -> String -> Http.Body -> Task.Task Http.Error a
post' dec url body =
    let
        _ =
            Debug.log ">>> Ready to POST" ( url, body )
    in
        Http.send Http.defaultSettings
            { verb = "POST"
            , headers = [ ( "Content-type", "application/json" ) ]
            , url = url
            , body = body
            }
            |> Http.fromJson dec



-- Full table queries


encodeCols : List ( String, a -> String ) -> Encode.Value
encodeCols columns =
    columns
        |> List.map (\( name, getter ) -> Encode.string name)
        |> Encode.list


loadTableHelper : Table a -> Decode.Decoder b -> (b -> Msg) -> Cmd Msg
loadTableHelper tbl dec successMsg =
    let
        body =
            [ ( "table_name", Encode.string tbl.name ), ( "columns", encodeCols tbl.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' dec (baseUrl ++ "/get-table") body)
            |> Task.perform DB_RequestFail successMsg


hospitalLoadCmd : Cmd Msg
hospitalLoadCmd =
    loadTableHelper hospitalTable (Decode.list hospitalDecoder) DB_HospitalTableSucceed


patientLoadCmd : Cmd Msg
patientLoadCmd =
    loadTableHelper patientTable (Decode.list patientDecoder) DB_PatientTableSucceed


diseaseLoadCmd : Cmd Msg
diseaseLoadCmd =
    loadTableHelper diseaseTable (Decode.list diseaseDecoder) DB_DiseaseTableSucceed



-- Login


validatePidCmd : String -> Cmd Msg
validatePidCmd pid =
    (post' Decode.bool (baseUrl ++ "/pid-login/" ++ pid) Http.empty)
        |> Task.perform DB_RequestFail (DB_PIDLoginSuccess pid)


validateMidCmd : String -> Cmd Msg
validateMidCmd mid =
    (post' Decode.bool (baseUrl ++ "/mid-login/" ++ mid) Http.empty)
        |> Task.perform DB_RequestFail (DB_MIDLoginSuccess mid)



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
                |> Encode.encode 1
                |> Http.string
    in
        (post' dec (baseUrl ++ "/query-fields") body)
            |> Task.perform DB_RequestFail successMsg


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


getNth : List a -> Int -> Maybe a
getNth items i =
    items
        |> Array.fromList
        |> Array.get i


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


sympByDiseaseCmd : String -> Cmd Msg
sympByDiseaseCmd virusName =
    let
        body =
            [ ( "virus_name", Encode.string virusName ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list symptomDecoder) (baseUrl ++ "/symptom-by-disease") body)
            |> Task.perform DB_RequestFail DB_SymptomSucceed


diseaseByHospitalCmd : String -> Cmd Msg
diseaseByHospitalCmd hospName =
    let
        body =
            [ ( "hospital_name", Encode.string hospName )
            , ( "columns", encodeCols diseaseTable.columns )
            ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list diseaseDecoder) (baseUrl ++ "/disease-by-hospital") body)
            |> Task.perform DB_RequestFail DB_DiseaseTableSucceed


patientByContactedSourceCmd : String -> Cmd Msg
patientByContactedSourceCmd pid =
    let
        body =
            [ ( "pid", Encode.string pid )
            , ( "columns", encodeCols patientTable.columns )
            ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list patientDecoder) (baseUrl ++ "/patient-by-contacted-source") body)
            |> Task.perform DB_RequestFail DB_OtherPatientTableSucceed



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
                |> Encode.encode 1
                |> Http.string
    in
        (post' medicDecoder (baseUrl ++ "/medic-me") body)
            |> Task.perform DB_RequestFail DB_MedicByMidSucceed


medicChecksOnCmd : Int -> Cmd Msg
medicChecksOnCmd mid =
    let
        body =
            [ ( "mid", Encode.int mid ), ( "columns", encodeCols patientTable.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list patientDecoder) (baseUrl ++ "/medic-checks-on") body)
            |> Task.perform DB_RequestFail DB_MedicChecksOnSucceed


medicsToDoCmd : Int -> Cmd Msg
medicsToDoCmd mid =
    let
        body =
            [ ( "mid", Encode.int mid ), ( "columns", encodeCols patientTable.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list patientDecoder) (baseUrl ++ "/medic-todo") body)
            |> Task.perform DB_RequestFail DB_MedicToDoSucceed


patientByPIDCmd : Int -> Cmd Msg
patientByPIDCmd pid =
    {- Get a single patient's row from patient -}
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols patientTable.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' patientDecoder (baseUrl ++ "/patient-me") body)
            |> Task.perform DB_RequestFail DB_PatientByPidSucceed


patientExhibitsCmd : Int -> Cmd Msg
patientExhibitsCmd pid =
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols symptomTable.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list symptomDecoder) (baseUrl ++ "/patient-exhibits") body)
            |> Task.perform DB_RequestFail DB_SymptomSucceed


patientHasCmd : Int -> Cmd Msg
patientHasCmd pid =
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols diseaseTable.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list diseaseDecoder) (baseUrl ++ "/patient-has") body)
            |> Task.perform DB_RequestFail DB_DiseaseTableSucceed


patientMedicCmd : Int -> Cmd Msg
patientMedicCmd pid =
    let
        body =
            [ ( "pid", Encode.int pid ), ( "columns", encodeCols medicTable.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' (Decode.list medicDecoder) (baseUrl ++ "/patient-medic") body)
            |> Task.perform DB_RequestFail DB_MedicTableSucceed



-- DECODERS


hospitalDecoder : Decode.Decoder Hospital
hospitalDecoder =
    decode Hospital
        |> required "hospital_name" string
        |> required "number_of_beds" string
        |> required "latitude" string
        |> required "longitude" string


patientDecoder : Decode.Decoder Patient
patientDecoder =
    decode Patient
        |> required "pid" string
        |> required "forename" string
        |> required "surname" string
        |> required "status" string
        |> required "phone_number" string
        |> required "dob" string
        |> required "latitude" string
        |> required "longitude" string


diseaseDecoder : Decode.Decoder Disease
diseaseDecoder =
    decode Disease
        |> required "virus_name" string
        |> required "incubation" string
        |> required "duration" string


symptomDecoder : Decode.Decoder Symptom
symptomDecoder =
    decode Symptom
        |> required "symptom_name" string
        |> required "description" string


medicDecoder : Decode.Decoder Medic
medicDecoder =
    decode Medic
        |> required "mid" string
        |> required "name" string
        |> required "phone_number" string
        |> required "hospital_name" string
        |> required "latitude" string
        |> required "longitude" string



-- VIEW
{-
   Three views' restrictions:

     Public: Restricts Patient view to just status, latitude, and longitude.
             Can join-search patients by disease.

     Patient: Me page includes all of that patient's data plus their list of
             contacts and their medic's data, excluding mid (optional).

     Medic: Me page includes that medic's data plus todo list of five
           closest patients. Can see all patient data.
-}


view : Model -> Html Msg
view model =
    div []
        [ pureCSS
        , localcss
        , div [ id "layout" ]
            [ Html.App.map UI_MenuAct (Menu.view menu model.mode)
            , loginView model
            , case model.mode of
                HospitalPage ->
                    div []
                        [ model.hospitals.list |> viewTable "Hospitals" True hospitalTable model.hospitals.sel
                        , model.diseases.list |> viewTable "Diseases Treated" False diseaseTable Nothing
                        ]

                PatientPage ->
                    div []
                        [ model.patients.list |> viewTable "Patients" True patientTable model.patients.sel
                        , model.otherPatients |> viewTable "Contacts" False patientTable Nothing
                        ]

                DiseasePage ->
                    div []
                        [ model.diseases.list |> viewTable "Diseases" True diseaseTable model.diseases.sel
                        , model.symptoms |> viewTable "Symptoms" False symptomTable Nothing
                        ]

                MyInfoPage ->
                    viewMyInfo model
            , h2 [] [ text model.mapCaption ]
            ]
        ]


loginView : Model -> Html Msg
loginView model =
    let
        loginMsg =
            case model.loginStatus of
                Public ->
                    ""

                PatientLoggedIn pid ->
                    "Logged in as patient " ++ (toString pid)

                MedicLoggedIn mid ->
                    "Logged in as medic " ++ (toString mid)
    in
        Html.div []
            [ Html.p []
                [ Html.label [ for model.loginNumberOptions.id ] [ text "Login ID" ]
                , Number.input model.loginNumberOptions
                    [ style
                        [ ( "border", "1px solid #ccc" )
                        , ( "padding", ".5rem" )
                        , ( "box-shadow", "inset 0 1px 1px rgba(0,0,0,.075);" )
                        ]
                    ]
                    model.loginNumberModel
                    |> Html.App.map UI_UpdateLogin
                , text loginMsg
                ]
            , div [ onClick UI_PIDLoginAttempt, class "pure-button" ] [ text "As Patient" ]
            , div [ onClick UI_MIDLoginAttempt, class "pure-button" ] [ text "As Medic" ]
            ]


viewTable : String -> Bool -> Table a -> Maybe Int -> List a -> Html Msg
viewTable header searchable tbl selIdx objects =
    let
        rowMsg =
            if tbl.name == "hospital" then
                UI_RowSelected_Hospital
            else if tbl.name == "patient" then
                UI_RowSelected_Patient
            else if tbl.name == "disease" then
                UI_RowSelected_Disease
            else
                UI_RowSelected_Ignore

        selectionIndex =
            case selIdx of
                Nothing ->
                    -99

                Just i ->
                    i

        rowAttrs i =
            let
                click =
                    if searchable then
                        [ onClick (rowMsg tbl.name i) ]
                    else
                        []

                sel =
                    if selectionIndex == i then
                        [ class "pure-table-odd" ]
                    else
                        []

                -- _ =
                --     Debug.log "row Attrs: selIdx, i, click, sel" ( selIdx, i, click, sel )
            in
                click ++ sel
    in
        table [ class "pure-table" ]
            [ caption []
                [ h2 [] [ text header ]
                , if searchable then
                    div [ class "pure-button pure-button-active search-button", onClick UI_DoFieldSearch ] [ text "search" ]
                  else
                    span [] []
                ]
            , thead []
                [ tr []
                    (tbl.columns
                        |> List.map
                            (\( name, getter ) ->
                                th []
                                    [ text name
                                    , if searchable then
                                        input [ onInput (UI_UpdateFieldInput name) ] []
                                      else
                                        span [] []
                                    ]
                            )
                    )
                  -- TODO: clear fields after search
                ]
            , tbody []
                (objects
                    |> List.indexedMap
                        (\ind obj ->
                            tr
                                (rowAttrs ind)
                                (List.map
                                    (\( colName, getter ) ->
                                        td [] [ text (getter obj) ]
                                    )
                                    tbl.columns
                                )
                        )
                )
            ]


viewMyInfo : Model -> Html Msg
viewMyInfo model =
    let
        _ =
            Debug.log "model.medicChecksOn length" (List.length model.medicChecksOn)

        _ =
            Debug.log "model.medicToDo length" (List.length model.medicToDo)
    in
        case model.mePage of
            MePageMedic m ->
                let
                    tbl =
                        medicTable
                in
                    div []
                        [ table [ class "pure-table" ]
                            [ caption []
                                [ h2 [] [ text ("My Information: " ++ tbl.name) ]
                                ]
                            , thead []
                                [ tr []
                                    (tbl.columns
                                        |> List.map
                                            (\( name, getter ) ->
                                                th []
                                                    [ text name
                                                    ]
                                            )
                                    )
                                ]
                            , tbody []
                                ([ m ]
                                    |> List.indexedMap
                                        (\ind obj ->
                                            tr []
                                                (List.map
                                                    (\( colName, getter ) ->
                                                        td [] [ text (getter obj) ]
                                                    )
                                                    tbl.columns
                                                )
                                        )
                                )
                            ]
                        , p []
                            [ model.medicChecksOn |> viewTable "Your Patients" False patientTable Nothing
                            ]
                        , p []
                            [ model.medicToDo |> viewTable "Today's todo list" False patientTable Nothing
                            ]
                        ]

            MePagePatient p ->
                let
                    tbl =
                        patientTable
                in
                    div []
                        [ table [ class "pure-table" ]
                            [ caption []
                                [ h2 [] [ text ("My Information: " ++ tbl.name) ]
                                ]
                            , thead []
                                [ tr []
                                    (tbl.columns
                                        |> List.map
                                            (\( name, getter ) ->
                                                th []
                                                    [ text name
                                                    ]
                                            )
                                    )
                                ]
                            , tbody []
                                ([ p ]
                                    |> List.indexedMap
                                        (\ind obj ->
                                            tr []
                                                (List.map
                                                    (\( colName, getter ) ->
                                                        td [] [ text (getter obj) ]
                                                    )
                                                    tbl.columns
                                                )
                                        )
                                )
                            ]
                        , Html.p []
                            [ model.medics |> viewTable "Your Doctor" False medicTable Nothing
                            ]
                        , Html.p []
                            [ model.symptoms |> viewTable "Your Symptoms" False symptomTable Nothing
                            ]
                        , Html.p []
                            [ model.diseases.list |> viewTable "Your Disease Diagnosis" False diseaseTable Nothing
                            ]
                        ]

            NoMePage ->
                div [] [ text "Please login to view your information." ]


pureCSS : Html a
pureCSS =
    node "link"
        [ rel "stylesheet"
        , type' "text/css"
        , href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
        ]
        []


localcss : Html a
localcss =
    node "link" [ rel "stylesheet", type' "text/css", href "localStyle.css" ] []
