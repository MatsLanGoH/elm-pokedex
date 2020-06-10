module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http



---- MODEL ----


initialModel =
    { queryString = ""
    , gotPokemon = False
    , pokemon = Pokemon "" 0 ""
    }


type alias Model =
    { queryString : String
    , gotPokemon : Bool
    , pokemon : Pokemon
    }


type alias Pokemon =
    { name : String
    , id : Int
    , type_1 : String
    }


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
                Ok responseText ->
                    ( Success responseText, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Elm Pokédex" ]
        , viewSearchBox model
        , viewResult model
        ]


viewSearchBox model =
    div []
        [ input [ type_ "text", placeholder "Enter a Pokémon name", value model.queryString, onInput UpdateQuery ] []
        , button [ onClick SubmitQuery ] [ text "Search" ]
        ]


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
