port module Main exposing (main)

import Error exposing (Error)
import Scanner exposing (Token)


port readFile : String -> Cmd msg


port readFileResult : (Maybe String -> msg) -> Sub msg


port userInput : (String -> msg) -> Sub msg


port print : String -> Cmd msg


port println : String -> Cmd msg


port exit : Int -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { args : List String }


type Model
    = WaitingForFileContents { filename : String }
    | ReplWaitingForInput
    | Done


type Msg
    = ReadFileResult { filename : String, contents : Maybe String }
    | GotUserInput String


exitWithMessage : Int -> String -> Cmd Msg
exitWithMessage code message =
    Cmd.batch
        [ println message
        , exit code
        ]


init : Flags -> ( Model, Cmd Msg )
init { args } =
    case args of
        [] ->
            runPrompt

        [ file ] ->
            runFile file

        _ ->
            ( Done
            , exitWithMessage 64 "Usage: jlox [script]"
            )


runFile : String -> ( Model, Cmd Msg )
runFile filename =
    ( WaitingForFileContents { filename = filename }
    , readFile filename
    )


runPrompt : ( Model, Cmd Msg )
runPrompt =
    ( ReplWaitingForInput
    , print "> "
    )


runAndRepeat : String -> ( Model, Cmd Msg )
runAndRepeat input =
    let
        ( cmd, maybeError ) =
            run input

        finalCmd =
            case maybeError of
                Nothing ->
                    cmd

                Just err ->
                    println (Error.toString err)
    in
    runPrompt
        |> addCmd finalCmd


addCmd : Cmd msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model, Cmd.batch [ cmd, oldCmd ] )


run : String -> ( Cmd Msg, Maybe Error )
run program =
    let
        tokens =
            Scanner.scanTokens program
    in
    ( print <| Debug.toString tokens
    , Nothing
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        WaitingForFileContents { filename } ->
            readFileResult
                (\contents ->
                    ReadFileResult
                        { filename = filename
                        , contents = contents
                        }
                )

        ReplWaitingForInput ->
            userInput GotUserInput

        Done ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadFileResult read ->
            case model of
                WaitingForFileContents waiting ->
                    if waiting.filename == read.filename then
                        case read.contents of
                            Nothing ->
                                ( Done
                                , exitWithMessage 64 ("Couldn't read file: " ++ read.filename)
                                )

                            Just contents ->
                                let
                                    ( cmd, maybeError ) =
                                        run contents
                                in
                                case maybeError of
                                    Nothing ->
                                        ( Done, cmd )

                                    Just err ->
                                        ( Done, exitWithMessage 65 (Error.toString err) )

                    else
                        -- throwing read file contents away
                        ( model, Cmd.none )

                ReplWaitingForInput ->
                    ( model, Cmd.none )

                Done ->
                    ( model, Cmd.none )

        GotUserInput input ->
            if String.isEmpty input then
                ( Done, Cmd.none )

            else
                runAndRepeat input
