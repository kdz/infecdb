module View exposing (..)

import Model exposing (..)
import Html exposing (button, text, div, Html, table, tr, td, node, h2, p, label, input, span, thead, th, tbody, caption)
import Html.Attributes exposing (style, href, rel, type_, class, id, for, src, attribute)
import Html.Events exposing (onClick, onInput)
import Menu
import Input.Number as Number


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
    let
        -- master-detail structure: Hospitals->Diseases, Diseases->Symptoms, Patients->Contacts
        selectedQualfier selList nameGetter =
            case selList.sel of
                Nothing ->
                    "(no selection above)"

                Just idx ->
                    case (getNth selList.list idx) of
                        Nothing ->
                            "!! Index Error !!"

                        Just x ->
                            " (" ++ (nameGetter x) ++ ")"
    in
        div []
            [ pureCSS
            , localcss
            , div [ id "layout" ]
                [ Html.map UI_MenuAct (Menu.view menu model.mode)
                , loginView model
                , case model.mode of
                    HospitalPage ->
                        div []
                            [ model.hospitals.list |> viewTable "Hospitals" True hospitalTable model.hospitals.sel
                            , model.diseases.list |> viewTable ("Diseases Treated" ++ selectedQualfier model.hospitals .hospitalName) False diseaseTable Nothing
                            ]

                    PatientPage ->
                        div []
                            [ model.patients.list |> viewTable "Patients" True patientTable model.patients.sel
                            , model.otherPatients |> viewTable ("Contacts" ++ selectedQualfier model.patients .pid) False patientTable Nothing
                            ]

                    DiseasePage ->
                        div []
                            [ model.diseases.list |> viewTable "Diseases" True diseaseTable model.diseases.sel
                            , model.symptoms |> viewTable ("Symptoms" ++ selectedQualfier model.diseases .virus_name) False symptomTable Nothing
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
                [ Html.label [] [ text "Login ID" ]
                  -- for model.loginNumberOptions.id
                , Number.input model.loginNumberOptions
                    [ style
                        [ ( "border", "1px solid #ccc" )
                        , ( "padding", ".5rem" )
                        , ( "box-shadow", "inset 0 1px 1px rgba(0,0,0,.075);" )
                        ]
                    ]
                    model.loginNumberModel
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
        , type_ "text/css"
        , href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
        ]
        []


localcss : Html a
localcss =
    node "link" [ rel "stylesheet", type_ "text/css", href "localStyle.css" ] []
