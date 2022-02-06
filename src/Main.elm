module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
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


type alias Input =
    { pat : String
    , repositoryName : String
    }


type alias Pull =
    { title : String
    , createdAt : String
    , number : Int
    , comments : List String
    }


type alias Model =
    { input : Input
    , pulls : List Pull
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input =
            { pat = ""
            , repositoryName = ""
            }
      , pulls = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | ButtonClick
    | GotPulls (Result Http.Error (List DecodedPull))
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


getCommentsFromPulls : String -> List DecodedPull -> Cmd Msg
getCommentsFromPulls pat pulls =
    Cmd.batch (List.map (\p -> getComments pat p.number) pulls)


updatePat : String -> Model -> Model
updatePat newPat model =
    let
        input =
            model.input

        newInput =
            { input | pat = newPat }
    in
    { model | input = newInput }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newPat ->
            ( updatePat newPat model, Cmd.none )

        ButtonClick ->
            ( model
            , getPulls model.input.pat
            )

        GotPulls result ->
            case result of
                Ok pulls ->
                    ( { model
                        | pulls =
                            List.foldr (::)
                                model.pulls
                                (List.map
                                    (\p ->
                                        { title = p.title
                                        , createdAt = p.createdAt
                                        , number = p.number
                                        , comments = []
                                        }
                                    )
                                    pulls
                                )
                      }
                    , getCommentsFromPulls model.input.pat pulls
                    )

                Err _ ->
                    ( model, Cmd.none )

        -- TODO: link comments to pulls. 
        GotComments result ->
            case result of
                Ok comments ->
                    ( { model | pulls = comments }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "your GitHub PAT", value model.input.pat, onInput Change ] []
        , button [ onClick ButtonClick ] [ text "Get comments!" ]
        , ul []
            (List.map
                (\p ->
                    li []
                        [ text (p.title ++ p.createdAt)
                        , text
                            (case List.head p.comments of
                                Just comment ->
                                    comment

                                Maybe.Nothing ->
                                    ""
                            )
                        ]
                )
                model.pulls
            )
        ]


type alias DecodedPull =
    { title : String
    , createdAt : String
    , number : Int
    }


pullsDecoder : Decoder (List DecodedPull)
pullsDecoder =
    field "items"
        (list
            (map3
                DecodedPull
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
