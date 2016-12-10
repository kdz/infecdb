module NewHttp exposing (..)

import Html exposing (button, text, div, Html, table, tr, td, node, h2, p, label, input, span, thead, th, tbody, caption)
import Html.Attributes exposing (style, href, rel, type_, class, id, for, src, attribute)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (string, float, int)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Http
import Result


main : Program Never Int Msg
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Foo
    | Bar (Result String Bool)


type Request a
    = Request a


post : String -> Http.Body -> Request a
post =
    Debug.crash "boom"


decodeResponseWith : Decode.Decoder a -> Request a -> ( Request a, Result String a )
decodeResponseWith =
    Debug.crash "boom"


handleWith : (Result String a -> Msg) -> ( Request a, Result String a ) -> Cmd Msg
handleWith =
    Debug.crash "boom"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Foo ->
            ( model
            , post "/doBar" (22 |> Encode.int |> Http.jsonBody)
                |> decodeResponseWith Decode.bool
                |> handleWith Bar
            )

        Bar (Ok val) ->
            ( model, Cmd.none )

        Bar (Err _) ->
            ( model, Cmd.none )


type alias Model =
    Int


view : Model -> Html Msg
view model =
    div [] [ text "blah" ]
