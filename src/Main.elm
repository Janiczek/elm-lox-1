port module Main exposing (main)


port readFile : String -> Cmd msg


port readFileResult : (Maybe String -> msg) -> Sub msg


port print : String -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { args : List String }


type alias Model =
    ()


type Msg
    = NoOp


init : Flags -> ( Model, Cmd msg )
init flags =
    ( (), print "Beam me up, Scotty!" )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
