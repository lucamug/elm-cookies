port module Main exposing (..)

--import Element.Background as Background
--import Element.Border as Border
--import Element.Events as Events
--import Element.Font as Font

import Element exposing (..)
import Html exposing (Html)
import Http
import Json.Decode
import Navigation
import Process
import Task
import Time


port fromAppToJs : String -> Cmd msg


port fromJsToApp : (String -> msg) -> Sub msg


type Msg
    = ChangeLocation String
    | NewApi2Data (Result Http.Error Api2Data)
    | FetchApi2Data String
    | HereAreYourCookies String


type alias Model =
    { api2Data : String
    , cookies : String
    }


requestCookies : Model -> Cmd msg
requestCookies model =
    fromAppToJs "give me cookies!"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "xxx" of
        ChangeLocation pathWithSlash ->
            ( model, Navigation.newUrl pathWithSlash )

        NewApi2Data result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model | api2Data = data.url }
                    in
                    ( newModel
                    , requestCookies newModel
                    )

                Err data ->
                    ( model, Cmd.none )

        FetchApi2Data url ->
            ( model, Http.send NewApi2Data (Http.get url api2Decoder) )

        HereAreYourCookies cookies ->
            ( { model | cookies = cookies }, Cmd.none )


view : Model -> Html Msg
view model =
    layout
        []
    <|
        el
            []
        <|
            text "ciao"


init : ( Model, Cmd Msg )
init =
    ( initModel
    , initCmd
    )


initModel : Model
initModel =
    { api2Data = ""
    , cookies = ""
    }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ Task.perform (\_ -> FetchApi2Data "https://httpbin.org/cookies/set?name2=value2") (Process.sleep (0.0 * Time.second))
        ]


type alias Api2Data =
    { url : String }


api2Decoder : Json.Decode.Decoder Api2Data
api2Decoder =
    Json.Decode.map Api2Data (Json.Decode.at [ "url" ] Json.Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromJsToApp HereAreYourCookies
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
