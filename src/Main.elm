module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


initial_model =
    { query_string = ""
    , pokemon = Pokemon "" 0 ""
    }


type alias Model =
    { query_string : String
    , pokemon : Pokemon
    }


type alias Pokemon =
    { name : String
    , id : Int
    , type_1 : String
    }


init : ( Model, Cmd Msg )
init =
    ( initial_model, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateQuery String
    | SubmitQuery


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newQuery ->
            ( { model | query_string = newQuery }
            , Cmd.none
            )

        SubmitQuery ->
            Debug.todo "Add Query submission"



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Elm Pokédex" ]
        , viewSearchBox model
        , viewPokemon model.pokemon
        ]


viewSearchBox model =
    div []
        [ input [ type_ "text", placeholder "Enter a Pokémon name", value model.query_string, onInput UpdateQuery ] []
        , button [ onClick SubmitQuery ] [ text "Search" ]
        ]


viewPokemon pokemon =
    div []
        [ h1 [] [ text "Result" ]
        , p [] [ text pokemon.name ]
        , p [] [ text ("id: " ++ String.fromInt pokemon.id) ]
        , p [] [ text ("type: " ++ pokemon.type_1) ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
