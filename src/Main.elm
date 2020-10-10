module Main exposing (main)

import Browser
import Html exposing (Html, button, code, div, h1, h2, h3, img, input, p, table, td, text, tr)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Random



---- CONFIG ----


baseApiUrl : String
baseApiUrl =
    "https://pokeapi.co/api/v2/pokemon/"



---- MODEL ----


initialModel : Model
initialModel =
    { queryString = ""
    , pokemon = NoPokemon
    , pokemons = []
    , apiResultStatus = NotLoaded
    }


type alias Model =
    { queryString : String
    , pokemon : Pokemon
    , pokemons : List PokemonSimple
    , apiResultStatus : ApiResultStatus
    }


type Pokemon
    = HasPokemon PokemonDetail
    | NoPokemon


type alias PokemonDetail =
    { name : String
    , id : Int
    , defaultMaleSprite : Maybe PokemonSprite
    , defaultFemaleSprite : Maybe PokemonSprite
    , shinyMaleSprite : Maybe PokemonSprite
    , shinyFemaleSprite : Maybe PokemonSprite
    }


type alias PokemonSprite =
    { frontUrl : String
    , backUrl : String
    , spriteType : String
    }


type alias PokemonSimple =
    { name : String
    , url : String
    }


type alias PokemonAll =
    { count : Int
    , results : List PokemonSimple
    }


pokemonSpriteDecoder : String -> Decoder (Maybe PokemonSprite)
pokemonSpriteDecoder str =
    let
        front_selector =
            "front_" ++ str

        back_selector =
            "back_" ++ str
    in
    Decode.oneOf
        [ Decode.map Just
            (Decode.succeed PokemonSprite
                |> required front_selector string
                |> required back_selector string
                |> hardcoded str
            )
        , Decode.succeed Nothing
        ]


pokemonDetailDecoder : Decoder PokemonDetail
pokemonDetailDecoder =
    Decode.succeed
        PokemonDetail
        |> required "name" string
        |> required "id" int
        |> required "sprites" (pokemonSpriteDecoder "default")
        |> required "sprites" (pokemonSpriteDecoder "shiny")
        |> required "sprites" (pokemonSpriteDecoder "female")
        |> required "sprites" (pokemonSpriteDecoder "shiny_female")


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
    | SurpriseMe
    | NewPokemonId Int
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
                | pokemon = NoPokemon
                , pokemons = []
                , apiResultStatus = Loading
              }
            , getPokemonDetail model.queryString
            )

        SearchAll ->
            ( { model
                | pokemon = NoPokemon
                , pokemons = []
                , apiResultStatus = Loading
              }
            , getPokemons
            )

        SurpriseMe ->
            ( model
            , Random.generate NewPokemonId (Random.int 1 800)
            )

        NewPokemonId pokemonId ->
            ( { model
                | pokemon = NoPokemon
                , pokemons = []
                , apiResultStatus = Loading
                , queryString = ""
              }
            , getPokemonDetail (String.fromInt pokemonId)
            )

        GotPokemonDetail (Ok pokemon) ->
            ( { model
                | apiResultStatus = Success "OK"
                , pokemon = HasPokemon pokemon
              }
            , Cmd.none
            )

        GotPokemonDetail (Err error) ->
            handleErrors model error

        GotPokemons (Ok pokemons) ->
            ( { model
                | apiResultStatus = Success "OK"
                , pokemons = pokemons.results
              }
            , Cmd.none
            )

        GotPokemons (Err error) ->
            handleErrors model error



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


handleErrors : Model -> Error -> ( Model, Cmd Msg )
handleErrors model error =
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

        surpriseMe =
            button [ onClick SurpriseMe ] [ text "Surprise me" ]
    in
    div []
        [ input [ type_ "text", placeholder "Enter a Pokémon name", value model.queryString, onInput UpdateQuery ] []
        , searchButton
        , surpriseMe
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
            div []
                [ h1 [] [ text "Result" ]
                , viewPokemon model.pokemon
                , viewPokemonList model.pokemons
                ]

        _ ->
            text ""



---- Pokemon Views -----


viewPokemon : Pokemon -> Html msg
viewPokemon pokemon =
    case pokemon of
        HasPokemon pokemonDetail ->
            let
                sprites =
                    [ pokemonDetail.defaultMaleSprite
                    , pokemonDetail.defaultFemaleSprite
                    , pokemonDetail.shinyMaleSprite
                    , pokemonDetail.shinyFemaleSprite
                    ]
                        |> List.filterMap identity

                viewSprites =
                    if List.length sprites > 0 then
                        let
                            spritesTable =
                                sprites
                                    |> List.map
                                        (\s ->
                                            tr []
                                                [ td []
                                                    [ h3 []
                                                        [ s.spriteType
                                                            |> String.split "_"
                                                            |> List.map String.toUpper
                                                            |> String.join " "
                                                            |> text
                                                        ]
                                                    ]
                                                , td [] [ img [ src s.frontUrl ] [] ]
                                                , td [] [ img [ src s.backUrl ] [] ]
                                                ]
                                        )
                                    |> table [ class "center" ]
                        in
                        div []
                            [ h3 []
                                [ "Sprites for "
                                    ++ String.toUpper pokemonDetail.name
                                    |> text
                                ]
                            , spritesTable
                            ]

                    else
                        div [] [ h3 [] [ text "No sprites for this Pokemon found." ] ]
            in
            div []
                [ h2 [] [ text "Pokemon Details" ]
                , p []
                    [ String.toUpper pokemonDetail.name
                        ++ " (#"
                        ++ String.fromInt pokemonDetail.id
                        ++ ")"
                        |> text
                    ]
                , viewSprites
                ]

        NoPokemon ->
            text ""


viewPokemonList : List PokemonSimple -> Html msg
viewPokemonList pokemons =
    pokemons
        |> List.map (\m -> p [] [ text m.name ])
        |> div []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
