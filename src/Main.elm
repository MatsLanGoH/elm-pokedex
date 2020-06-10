module Main exposing (main)

import Browser
import Html exposing (Html, button, code, div, h1, img, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, field, int, map3, string)



---- CONFIG ----


baseApiUrl : String
baseApiUrl =
    "https://pokeapi.co/api/v2/pokemon/"



---- MODEL ----


initialModel : Model
initialModel =
    { queryString = ""
    , pokemon = Pokemon "" 0 ""
    , apiResultStatus = NotLoaded
    }


type alias Model =
    { queryString : String
    , pokemon : Pokemon
    , apiResultStatus : ApiResultStatus
    }


type alias Pokemon =
    { name : String
    , id : Int
    , sprite_front_default_url : String
    }


pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    map3 Pokemon
        (field "name" string)
        (field "id" int)
        (field "sprites" (field "front_default" string))


type ApiResultStatus
    = NotLoaded
    | Loading
    | Failure String
    | Success String


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateQuery String
    | SubmitQuery
    | GotPokemon (Result Http.Error Pokemon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newQuery ->
            ( { model | queryString = newQuery }
            , Cmd.none
            )

        SubmitQuery ->
            ( { model | apiResultStatus = Loading }
            , getPokemon model.queryString
            )

        GotPokemon result ->
            case result of
                Ok pokemon ->
                    ( { model | apiResultStatus = Success "OK", pokemon = pokemon }, Cmd.none )

                Err error ->
                    let
                        message =
                            case error of
                                BadUrl url ->
                                    "Invalid URL " ++ url

                                Timeout ->
                                    "Connection has timed out"

                                NetworkError ->
                                    "Unable to reach the server"

                                BadStatus statusCode ->
                                    "Status Error: " ++ String.fromInt statusCode

                                BadBody errorMessage ->
                                    errorMessage
                    in
                    ( { model | apiResultStatus = Failure message }, Cmd.none )



---- HTTP ----


getPokemon : String -> Cmd Msg
getPokemon queryString =
    Http.get
        { url = baseApiUrl ++ String.toLower queryString
        , expect = Http.expectJson GotPokemon pokemonDecoder
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Elm Pokédex" ]
        , viewSearchBox model
        , viewApiResultStatus model
        , viewResult model
        ]


viewSearchBox : Model -> Html Msg
viewSearchBox model =
    div []
        [ input [ type_ "text", placeholder "Enter a Pokémon name", value model.queryString, onInput UpdateQuery ] []
        , button [ onClick SubmitQuery ] [ text "Search" ]
        ]


viewApiResultStatus : Model -> Html msg
viewApiResultStatus model =
    let
        apiResultStatusItem =
            case model.apiResultStatus of
                NotLoaded ->
                    text "まだ何も検索していないYo"

                Loading ->
                    text "検索中だYo..."

                Failure err ->
                    div []
                        [ p [] [ text "失敗したYo :-(" ]
                        , code [] [ text err ]
                        ]

                Success result ->
                    div []
                        [ p [] [ text "見つけたYo :)" ]
                        , code [] [ text result ]
                        ]
    in
    div []
        [ h1 [] [ text "検索ステータス" ]
        , apiResultStatusItem
        ]


viewResult : Model -> Html msg
viewResult model =
    case model.apiResultStatus of
        Success _ ->
            let
                pokemon =
                    model.pokemon
            in
            div []
                [ h1 [] [ text "Result" ]
                , p []
                    [ text
                        (String.toUpper pokemon.name
                            ++ " (#"
                            ++ String.fromInt pokemon.id
                            ++ ")"
                        )
                    ]
                , img [ src pokemon.sprite_front_default_url ] []
                ]

        _ ->
            div []
                [ h1 [] [ text "Nothing found" ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
