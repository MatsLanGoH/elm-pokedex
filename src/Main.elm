module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http



---- CONFIG ----


baseApiUrl : String
baseApiUrl =
    "https://pokeapi.co/api/v2/pokemon/"



---- MODEL ----


initialModel : Model
initialModel =
    { queryString = ""
    , gotPokemon = False
    , pokemon = Pokemon "" 0 ""
    , apiResultStatus = NotLoaded
    }


type alias Model =
    { queryString : String
    , gotPokemon : Bool
    , pokemon : Pokemon
    , apiResultStatus : ApiResultStatus
    }


type alias Pokemon =
    { name : String
    , id : Int
    , type_1 : String
    }


type ApiResultStatus
    = NotLoaded
    | Loading
    | Failure
    | Success String


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateQuery String
    | SubmitQuery
    | GotApiResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newQuery ->
            ( { model | queryString = newQuery }
            , Cmd.none
            )

        SubmitQuery ->
            Debug.todo "Add Query submission"

        GotApiResponse result ->
            case result of
                Ok resultText ->
                    ( { model | apiResultStatus = Success resultText }, Cmd.none )

                Err _ ->
                    ( { model | apiResultStatus = Failure }, Cmd.none )



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
        apiResultStatusText =
            case model.apiResultStatus of
                NotLoaded ->
                    "まだ何も検索していないYo"

                Loading ->
                    "検索中だYo..."

                Failure ->
                    "失敗したYo :-("

                Success _ ->
                    "見つけたYo :)"
    in
    div []
        [ h1 [] [ text "検索ステータス" ]
        , text apiResultStatusText
        ]


viewResult : Model -> Html msg
viewResult model =
    if model.gotPokemon == True then
        let
            pokemon =
                model.pokemon
        in
        div []
            [ h1 [] [ text "Result" ]
            , p [] [ text ("name" ++ pokemon.name) ]
            , p [] [ text ("id: " ++ String.fromInt pokemon.id) ]
            , p [] [ text ("type: " ++ pokemon.type_1) ]
            ]

    else
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
