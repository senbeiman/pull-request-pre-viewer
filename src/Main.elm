module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, pre, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type HttpStatus
    = Failure
    | Loading
    | Success String
    | Idle


type alias Model =
    { content : String
    , httpStatus : HttpStatus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = ""
      , httpStatus = Idle
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | ButtonClick
    | GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        ButtonClick ->
            ( { model | httpStatus = Loading }
            , Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , expect = Http.expectString GotText
                }
            )

        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | httpStatus = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | httpStatus = Failure }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "your GitHub PAT", value model.content, onInput Change ] []
        , button [ onClick ButtonClick ] [ text "Get comments!" ]
        , case model.httpStatus of
            Failure ->
                text "I was unable to load your comments."

            Loading ->
                text "Loading..."

            Idle ->
                text ""

            Success fullText ->
                pre [] [ text fullText ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
