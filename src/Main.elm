module Main exposing (main)

import Browser
import Html exposing (Html, button, code, div, h1, h2, img, input, p, text)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, at, decodeString, field, int, list, map, map3, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)



---- CONFIG ----


baseApiUrl : String
baseApiUrl =
    "https://pokeapi.co/api/v2/pokemon/"



---- MODEL ----


initialModel : Model
initialModel =
    { queryString = ""
    , pokemon = initialPokemon
    , pokemons = []
    , apiResultStatus = NotLoaded
    }


initialPokemon : PokemonDetail
initialPokemon =
    PokemonDetail
        ""
        0
        (PokemonSprite "" "" "" "" "" "" "" "")


type alias Model =
    { queryString : String
    , pokemon : PokemonDetail
    , pokemons : List PokemonSimple
    , apiResultStatus : ApiResultStatus
    }


type alias PokemonDetail =
    { name : String
    , id : Int
    , sprites : PokemonSprite
    }


type alias PokemonSprite =
    { front_default : String
    , back_default : String
    , front_female : String
    , back_female : String
    , front_shiny : String
    , back_shiny : String
    , front_shiny_female : String
    , back_shiny_female : String
    }


type alias PokemonSimple =
    { name : String
    , url : String
    }


type alias PokemonAll =
    { count : Int
    , results : List PokemonSimple
    }


pokemonSpriteDecoder : Decoder PokemonSprite
pokemonSpriteDecoder =
    Decode.succeed
        PokemonSprite
        |> required "front_default" string
        |> required "back_default" string
        |> required "front_female" string
        |> required "back_female" string
        |> required "front_shiny" string
        |> required "back_shiny" string
        |> required "front_shiny_female" string
        |> required "back_shiny_female" string


pokemonDetailDecoder : Decoder PokemonDetail
pokemonDetailDecoder =
    Decode.succeed
        PokemonDetail
        |> required "name" string
        |> required "id" int
        |> required "sprites" pokemonSpriteDecoder


pokemonSimpleDecoder : Decoder PokemonSimple
pokemonSimpleDecoder =
    Decode.succeed
        PokemonSimple
        |> required "name" string
        |> required "url" string


pokemonAllDecoder : Decoder PokemonAll
pokemonAllDecoder =
    Decode.succeed
        PokemonAll
        |> required "count" int
        |> required "results" (Decode.list pokemonSimpleDecoder)


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
    | SearchAll
    | GotPokemonDetail (Result Http.Error PokemonDetail)
    | GotPokemons (Result Http.Error PokemonAll)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newQuery ->
            ( { model | queryString = newQuery }
            , Cmd.none
            )

        SubmitQuery ->
            ( { model
                | pokemon = initialPokemon
                , pokemons = []
                , apiResultStatus = Loading
              }
            , getPokemonDetail model.queryString
            )

        SearchAll ->
            ( { model
                | pokemon = initialPokemon
                , pokemons = []
                , apiResultStatus = Loading
              }
            , getPokemons
            )

        GotPokemonDetail result ->
            case result of
                Ok pokemon ->
                    ( { model
                        | apiResultStatus = Success "OK"
                        , pokemon = pokemon
                      }
                    , Cmd.none
                    )

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

        GotPokemons result ->
            case result of
                Ok pokemons ->
                    ( { model
                        | apiResultStatus = Success "OK"
                        , pokemons = pokemons.results
                      }
                    , Cmd.none
                    )

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
                    ( { model
                        | apiResultStatus = Failure message
                      }
                    , Cmd.none
                    )



---- HTTP ----


getPokemonDetail : String -> Cmd Msg
getPokemonDetail queryString =
    Http.get
        { url = baseApiUrl ++ String.toLower queryString
        , expect = Http.expectJson GotPokemonDetail pokemonDetailDecoder
        }


getPokemons : Cmd Msg
getPokemons =
    Http.get
        { url = baseApiUrl
        , expect = Http.expectJson GotPokemons pokemonAllDecoder
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
    let
        searchButton =
            case String.length model.queryString of
                0 ->
                    button [ onClick SearchAll ] [ text "Search all" ]

                _ ->
                    button [ onClick SubmitQuery ] [ text "Search one" ]
    in
    div []
        [ input [ type_ "text", placeholder "Enter a Pokémon name", value model.queryString, onInput UpdateQuery ] []
        , searchButton
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

                sprites =
                    model.pokemon.sprites

                viewPokemon =
                    if String.length pokemon.name > 0 then
                        div []
                            [ p []
                                [ text
                                    (String.toUpper pokemon.name
                                        ++ " (#"
                                        ++ String.fromInt pokemon.id
                                        ++ ")"
                                    )
                                ]
                            , div []
                                [ h2 [] [ text "Male" ]
                                , img [ src sprites.front_default ] []
                                , img [ src sprites.back_default ] []
                                ]
                            , div []
                                [ h2 [] [ text "Female" ]
                                , img [ src sprites.front_female ] []
                                , img [ src sprites.back_female ] []
                                ]
                            , div []
                                [ h2 [] [ text "Shiny Male" ]
                                , img [ src sprites.front_shiny ] []
                                , img [ src sprites.back_shiny ] []
                                ]
                            , div []
                                [ h2 [] [ text "Shiny Female" ]
                                , img [ src sprites.front_shiny_female ] []
                                , img [ src sprites.back_shiny_female ] []
                                ]
                            ]

                    else
                        div [] []

                viewPokemonList =
                    model.pokemons
                        |> List.map (\m -> p [] [ text m.name ])
                        |> div []
            in
            div []
                [ h1 [] [ text "Result" ]
                , viewPokemon
                , viewPokemonList
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
