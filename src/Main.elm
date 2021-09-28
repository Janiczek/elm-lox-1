port module Main exposing (main)

import Error exposing (Error)
import Scanner


port readFile : String -> Cmd msg


port readFileResult : (Maybe String -> msg) -> Sub msg


port userInput : (String -> msg) -> Sub msg


port print : String -> Cmd msg


port println : String -> Cmd msg


port exitWithMessage : ( Int, String ) -> Cmd msg


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


init : Flags -> ( Model, Cmd Msg )
init { args } =
    case args of
        [] ->
            runPrompt

        [ file ] ->
            runFile file

        _ ->
            ( Done
            , exitWithMessage ( 64, "Usage: jlox [script]" )
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
        finalCmd : Cmd Msg
        finalCmd =
            case run input of
                Ok cmd ->
                    cmd

                Err err ->
                    println (String.join "\n" (List.map Error.toString err))
    in
    runPrompt
        |> addCmd finalCmd


addCmd : Cmd msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , oldCmd
        ]
    )


run : String -> Result (List Error) (Cmd Msg)
run program =
    -- TODO return values here instead of Cmd?
    Scanner.scanTokens program
        |> Result.map (println << Debug.toString)


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
                                , exitWithMessage ( 64, "Couldn't read file: " ++ read.filename )
                                )

                            Just contents ->
                                case run contents of
                                    Ok cmd ->
                                        ( Done, cmd )

                                    Err err ->
                                        ( Done, exitWithMessage ( 65, String.join "\n" (List.map Error.toString err) ) )

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
