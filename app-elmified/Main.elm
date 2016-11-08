module Main exposing (..)

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


-- import Json.Encode as Json

import Task exposing (perform)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { hospitals : List Hospital
    , patients : List Patient
    , diseases : List Disease
    , symptoms : List Symptom
    , mode : Mode
    , loginStatus : Status
    , loginNumberModel : Number.Model
    , loginNumberOptions : Number.Options
    , fields : List Field
    }


initModel : Model
initModel =
    { hospitals = []
    , patients = []
    , diseases = []
    , symptoms = []
    , mode = HospitalPage
    , loginStatus = Public
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
      -- (TODO)
    | MePage


type Status
    = Public
    | PatientLoggedIn Int
    | MedicLoggedIn Int


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
    , ( "Me", MePage )
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


type Msg
    = HospitalTableSucceed (List Hospital)
    | RequestFail Http.Error
    | MenuAct (Menu.Msg Mode)
    | UpdateLogin Number.Msg
    | PIDLoginAttempt
    | MIDLoginAttempt
    | PIDLoginSuccess Bool
    | MIDLoginSuccess Bool
    | PatientTableSucceed (List Patient)
    | DiseaseTableSucceed (List Disease)
    | SymptomByDiseaseSucceed (List Symptom)
    | UpdateFieldInput String String
    | FieldSearch
    | NoJoin String Int
    | DiseaseByHospital String Int
    | SymptomByDisease String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        result =
            case msg of
                HospitalTableSucceed incomingHospitals ->
                    ( { model
                        | hospitals = incomingHospitals
                        , fields = tableFields hospitalTable
                      }
                    , Cmd.none
                    )

                PatientTableSucceed incomingPatients ->
                    ( { model
                        | patients = incomingPatients
                        , fields = tableFields patientTable
                      }
                    , Cmd.none
                    )

                DiseaseTableSucceed incomingDiseases ->
                    ( { model
                        | diseases = incomingDiseases
                        , fields = tableFields diseaseTable
                      }
                    , Cmd.none
                    )

                SymptomByDiseaseSucceed incomingSymptoms ->
                    ( { model | symptoms = incomingSymptoms }, Cmd.none )

                RequestFail err ->
                    ( model, Cmd.none )

                MenuAct (Menu.Select mode) ->
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

                            _ ->
                                ( { model | mode = newMode }, Cmd.none )

                UpdateLogin numberMsg ->
                    ( { model
                        | loginNumberModel = Number.update numberMsg model.loginNumberModel
                      }
                    , Cmd.none
                    )

                PIDLoginAttempt ->
                    ( model, validatePidCmd model.loginNumberModel.value )

                MIDLoginAttempt ->
                    ( model, validateMidCmd model.loginNumberModel.value )

                PIDLoginSuccess success ->
                    case success of
                        True ->
                            ( { model
                                | loginStatus =
                                    PatientLoggedIn
                                        ((String.toInt model.loginNumberModel.value)
                                            |> Result.withDefault 0
                                        )
                              }
                            , Cmd.none
                            )

                        False ->
                            ( model, Cmd.none )

                MIDLoginSuccess success ->
                    case success of
                        True ->
                            ( { model
                                | loginStatus =
                                    MedicLoggedIn
                                        ((String.toInt model.loginNumberModel.value)
                                            |> Result.withDefault 0
                                        )
                              }
                            , Cmd.none
                            )

                        False ->
                            ( model, Cmd.none )

                UpdateFieldInput field val ->
                    ( { model | fields = updateField field val model.fields }, Cmd.none )

                FieldSearch ->
                    case model.mode of
                        HospitalPage ->
                            ( model, queryHospitalFieldsCmd model )

                        PatientPage ->
                            ( model, queryPatientFieldsCmd model )

                        DiseasePage ->
                            ( model, queryDiseaseFieldsCmd model )

                        _ ->
                            ( model, Cmd.none )

                NoJoin x i ->
                    ( model, Cmd.none )

                DiseaseByHospital tblName ind ->
                    ( model, diseaseByHospitalCmd (primaryKey model tblName ind) )

                SymptomByDisease tblName ind ->
                    ( model, sympByDiseaseCmd (primaryKey model tblName ind) )

        _ =
            Debug.log "update: " ( msg, model.mode, model.loginStatus )
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
    "http://localhost:5000"



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


loadTable : Table a -> Decode.Decoder b -> (b -> Msg) -> Cmd Msg
loadTable tbl dec successMsg =
    let
        body =
            [ ( "table_name", Encode.string tbl.name ), ( "columns", encodeCols tbl.columns ) ]
                |> Encode.object
                |> Encode.encode 1
                |> Http.string
    in
        (post' dec (baseUrl ++ "/get-table") body)
            |> Task.perform RequestFail successMsg


hospitalLoadCmd : Cmd Msg
hospitalLoadCmd =
    loadTable hospitalTable (Decode.list hospitalDecoder) HospitalTableSucceed


patientLoadCmd : Cmd Msg
patientLoadCmd =
    loadTable patientTable (Decode.list patientDecoder) PatientTableSucceed


diseaseLoadCmd : Cmd Msg
diseaseLoadCmd =
    loadTable diseaseTable (Decode.list diseaseDecoder) DiseaseTableSucceed



-- Login


validatePidCmd : String -> Cmd Msg
validatePidCmd pid =
    (Http.post Decode.bool (baseUrl ++ "/pid-login/" ++ pid) Http.empty)
        |> Task.perform RequestFail PIDLoginSuccess


validateMidCmd : String -> Cmd Msg
validateMidCmd mid =
    (Http.post Decode.bool (baseUrl ++ "/mid-login/" ++ mid) Http.empty)
        |> Task.perform RequestFail MIDLoginSuccess



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
            |> Task.perform RequestFail successMsg


queryHospitalFieldsCmd : Model -> Cmd Msg
queryHospitalFieldsCmd model =
    queryFieldsCmd hospitalTable model.fields (Decode.list hospitalDecoder) HospitalTableSucceed


queryPatientFieldsCmd : Model -> Cmd Msg
queryPatientFieldsCmd model =
    queryFieldsCmd patientTable model.fields (Decode.list patientDecoder) PatientTableSucceed


queryDiseaseFieldsCmd : Model -> Cmd Msg
queryDiseaseFieldsCmd model =
    queryFieldsCmd diseaseTable model.fields (Decode.list diseaseDecoder) DiseaseTableSucceed



-- Join queries


getNth : List a -> Int -> Maybe a
getNth items i =
    -- items
    --     |> List.indexedMap (\ind elem -> ( ind, elem ))
    --     |> List.filter (\( ind, elem ) -> ind == i)
    --     |> List.map (\( ind, elem ) -> elem)
    --     |> List.head
    items
        |> Array.fromList
        |> Array.get i


primaryKey : Model -> String -> Int -> String
primaryKey model tblName ind =
    {- Only handles hospital and disease cases -}
    if tblName == "hospital" then
        let
            h =
                (getNth model.hospitals ind)
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
                (getNth model.diseases ind)
        in
            case d of
                Just dis ->
                    dis
                        |> .virus_name

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
            |> Task.perform RequestFail SymptomByDiseaseSucceed


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
            |> Task.perform RequestFail DiseaseTableSucceed



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
        [ scriptWebComponents
        , importGoogleMapComponent
        , pureCSS
        , localcss
        , div [] [ text (toString model.fields) ]
          -- (TODO) remove when done field-querying
        , div [ id "layout" ]
            [ Html.App.map MenuAct (Menu.view menu model.mode)
            , loginView model
            , case model.mode of
                HospitalPage ->
                    div []
                        [ model.hospitals |> viewTable hospitalTable
                        , model.diseases |> viewTable diseaseTable
                        ]

                PatientPage ->
                    model.patients |> viewTable patientTable

                DiseasePage ->
                    div []
                        [ model.diseases |> viewTable diseaseTable
                        , model.symptoms |> viewTable symptomTable
                        ]

                _ ->
                    div [] [ text "To be implemented!" ]
            , googleMap
                [ attribute "latitude" "40.793575"
                , attribute "longitude" "-73.950564"
                ]
                -- Can add google-map-marker child elements https://goo.gl/CDznwU
                []
            ]
        ]


loginView : Model -> Html Msg
loginView model =
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
                |> Html.App.map UpdateLogin
            , case model.loginStatus of
                Public ->
                    div [] [ text "not yet logged in" ]

                PatientLoggedIn pid ->
                    div [] [ text ("logged in as Patient #" ++ (toString pid)) ]

                MedicLoggedIn mid ->
                    div [] [ text ("logged in as Medic #" ++ (toString mid)) ]
            ]
        , div [ onClick PIDLoginAttempt, class "pure-button" ] [ text "As Patient" ]
        , div [ onClick MIDLoginAttempt, class "pure-button" ] [ text "As Medic" ]
        ]


viewTable : Table a -> List a -> Html Msg
viewTable tbl objects =
    let
        rowMsg =
            if tbl.name == "hospital" then
                DiseaseByHospital
            else if tbl.name == "patient" then
                NoJoin
            else if tbl.name == "disease" then
                SymptomByDisease
            else
                NoJoin
    in
        table [ class "pure-table" ]
            [ caption []
                [ h2 [] [ text tbl.name ]
                , div [ class "pure-button pure-button-active", onClick FieldSearch ] [ text "search" ]
                ]
            , thead []
                [ tr []
                    (tbl.columns
                        |> List.map
                            (\( name, getter ) ->
                                th []
                                    [ text name
                                    , input [ onInput (UpdateFieldInput name) ] []
                                    ]
                            )
                    )
                  -- TODO: clear fields after search
                ]
            , tbody []
                (objects
                    |> List.indexedMap
                        (\ind obj ->
                            tr [ onClick (rowMsg tbl.name ind) ]
                                (List.map
                                    (\( colName, getter ) ->
                                        td [] [ text (getter obj) ]
                                    )
                                    tbl.columns
                                )
                        )
                )
            ]


scriptWebComponents : Html a
scriptWebComponents =
    node "script" [ src "bower_components/webcomponentsjs/webcomponents-lite.min.js" ] []


importGoogleMapComponent : Html a
importGoogleMapComponent =
    node "link" [ rel "import", href "bower_components/google-map/google-map.html" ] []


googleMap : List (Html.Attribute a) -> List (Html a) -> Html a
googleMap =
    -- TODO: NEED API KEYS
    Html.node "google-map"


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
