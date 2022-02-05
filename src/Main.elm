module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, string)
import List



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
    | PullsSuccess (List Pull)
    | CommentsSuccess (List String)
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
    | GotComments (Result Http.Error (List String))


getPulls : String -> Cmd Msg
getPulls pat =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("token " ++ pat)
            , Http.header "Accept" "application/vnd.github.v3+json"
            , Http.header "Content-Type" "application/json"
            ]
        , url = "https://api.github.com/search/issues?q=is:pr+author:senbeiman+org:xflagstudio&sort=created&order=asc"
        , expect = Http.expectJson GotPulls pullsDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


getComments : String -> Int -> Cmd Msg
getComments pat pullNumber =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("token " ++ pat)
            , Http.header "Accept" "application/vnd.github.v3+json"
            , Http.header "Content-Type" "application/json"
            ]
        , url = "https://api.github.com/repos/xflagstudio/fansta-api/pulls/" ++ String.fromInt pullNumber ++ "/comments"
        , expect = Http.expectJson GotComments commentsDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


getCommentsFromPulls : String -> List Pull -> Cmd Msg
getCommentsFromPulls pat pulls =
    Cmd.batch (List.map (\p -> getComments pat p.number) pulls)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        ButtonClick ->
            ( { model | httpStatus = Loading }
            , getPulls model.content
            )

        GotPulls result ->
            case result of
                Ok pulls ->
                    ( { model | httpStatus = PullsSuccess pulls }, getCommentsFromPulls model.content pulls )

                Err _ ->
                    ( { model | httpStatus = Failure }, Cmd.none )

        -- TODO: link comments to pulls. needs to change type of model
        GotComments result ->
            case result of
                Ok comments ->
                    ( { model | httpStatus = CommentsSuccess comments }, Cmd.none )

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

            PullsSuccess pulls ->
                ul [] (List.map (\l -> li [] [ text (l.title ++ l.createdAt) ]) pulls)

            -- TODO: show comments linked with pulls
            CommentsSuccess comments ->
                ul [] (List.map (\l -> li [] [ text l ]) comments)
        ]


type alias Pull =
    { title : String
    , createdAt : String
    , number : Int
    }


pullsDecoder : Decoder (List Pull)
pullsDecoder =
    field "items"
        (list
            (map3
                Pull
                (field "title" string)
                (field "created_at" string)
                (field "number" int)
            )
        )


commentsDecoder : Decoder (List String)
commentsDecoder =
    list
        (field "body" string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
