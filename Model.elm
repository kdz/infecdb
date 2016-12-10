module Model exposing (..)

import Input.Number as Number
import Http
import Menu
import Array


-- MODEL


type alias Location =
    { lat : Float, lng : Float }


type alias SelList a =
    { list : List a, sel : Maybe Int }


type alias Model =
    { hospitals : SelList Hospital
    , diseases : SelList Disease
    , patients : SelList Patient
    , otherPatients : List Patient
    , medicChecksOn : List Patient
    , medicToDo : List Patient
    , locationsForMap : List Location
    , mapCaption : String
    , symptoms : List Symptom
    , medics : List Medic
    , mode : Mode
    , loginStatus : Status
    , mePage : MePage
    , loginNumberModel : Maybe Int
    , loginNumberOptions : Number.Options Msg
    , fields : List Field
    }


initModel : Model
initModel =
    { hospitals = { list = [], sel = Nothing }
    , diseases = { list = [], sel = Nothing }
    , patients = { list = [], sel = Nothing }
    , otherPatients = []
    , medicChecksOn = []
    , medicToDo = []
    , locationsForMap = []
    , mapCaption = ""
    , symptoms = []
    , medics = []
    , mode = HospitalPage
    , loginStatus = Public
    , mePage = NoMePage
    , loginNumberModel = Nothing
    , loginNumberOptions = Number.defaultOptions UI_UpdateLogin
    , fields = tableFields hospitalTable
    }


getNth : List a -> Int -> Maybe a
getNth items i =
    items
        |> Array.fromList
        |> Array.get i


type Msg
    = -- DB ---------
      DB_HospitalTableSucceed (List Hospital)
    | DB_RequestFail Http.Error
    | DB_PIDLoginResult String Bool
    | DB_MIDLoginResult String Bool
    | DB_PatientTableSucceed (List Patient)
    | DB_OtherPatientTableSucceed (List Patient)
    | DB_MedicToDoSucceed (List Patient)
    | DB_MedicChecksOnSucceed (List Patient)
    | DB_DiseaseTableSucceed (List Disease)
    | DB_SymptomSucceed (List Symptom)
    | DB_MedicTableSucceed (List Medic)
    | DB_MedicByMidSucceed Medic
    | DB_PatientByPidSucceed Patient
      -- UI -------------
    | UI_MenuAct (Menu.Msg Mode)
    | UI_UpdateLogin (Maybe Int)
    | UI_PIDLoginAttempt
    | UI_MIDLoginAttempt
    | UI_UpdateFieldInput String String
    | UI_DoFieldSearch
    | UI_RowSelected_Ignore String Int
    | UI_RowSelected_Hospital String Int
    | UI_RowSelected_Disease String Int
    | UI_RowSelected_Patient String Int


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
