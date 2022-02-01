module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map2, string)
import List exposing (map)



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
    | Success (List Pull)
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
    | GotPulls (Result Http.Error (List Pull))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        ButtonClick ->
            ( { model | httpStatus = Loading }
            , Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" ("token " ++ model.content)
                    , Http.header "Accept" "application/vnd.github.v3+json"
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = "https://api.github.com/search/issues?q=is:pr+author:senbeiman+org:xflagstudio&sort=created&order=asc"
                , expect = Http.expectJson GotPulls pullsDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotPulls result ->
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

            Success pulls ->
                ul [] (map (\l -> li [] [ text (l.title ++ l.createdAt) ]) pulls)
        ]


type alias Pull =
    { title : String
    , createdAt : String
    }


pullsDecoder : Decoder (List Pull)
pullsDecoder =
    field "items" (list testDecoder)


testDecoder : Decoder Pull
testDecoder =
    map2
        Pull
        (field "title" string)
        (field "created_at" string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
