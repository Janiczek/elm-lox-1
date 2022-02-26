port module Main exposing (main)

import AstPrinter
import Effect exposing (Effect(..))
import Error exposing (Error)
import Interpreter
import Parser
import Scanner
import Token


port readFile : String -> Cmd msg


port readFileResult : (Maybe String -> msg) -> Sub msg


port waitForUserInput : () -> Cmd msg


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
    , Cmd.batch
        [ print "> "
        , waitForUserInput ()
        ]
    )


runAndRepeat : String -> ( Model, Cmd Msg )
runAndRepeat input =
    let
        _ =
            case run input of
                Ok effects ->
                    let
                        _ =
                            effects
                                |> List.foldl (\eff () -> runEffect eff) ()
                    in
                    ()

                Err ( errors, effects ) ->
                    let
                        _ =
                            effects
                                |> List.foldl (\eff () -> runEffect eff) ()
                    in
                    errors
                        |> Error.toString
                        |> Debug.log "err"
                        |> always ()
    in
    runPrompt


logList : (a -> b) -> String -> List a -> List a
logList fn label list =
    list
        |> List.reverse
        |> List.map
            (\x ->
                let
                    _ =
                        Debug.log label (fn x)
                in
                x
            )
        |> List.reverse


run : String -> Result ( Error, List Effect ) (List Effect)
run program =
    let
        scan =
            Scanner.scan >> Result.map (logList Token.lexeme "scanned")

        parse =
            Parser.parseProgram
                >> Result.map (logList AstPrinter.printStatement "parsed")

        addEffects =
            Result.mapError (\err -> ( err, [] ))

        interpret =
            Interpreter.interpretProgram
                >> Result.map (logList identity "interpreted")
    in
    program
        |> scan
        |> Result.andThen parse
        |> addEffects
        |> Result.andThen interpret


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
            readFileResult_ read model

        GotUserInput input ->
            if String.isEmpty input then
                ( Done, Cmd.none )

            else
                runAndRepeat input


runEffect : Effect -> ()
runEffect effect =
    case effect of
        PrintEff string ->
            let
                _ =
                    Debug.log string "[PRINT]"
            in
            ()


readFileResult_ : { filename : String, contents : Maybe String } -> Model -> ( Model, Cmd Msg )
readFileResult_ read model =
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
                            Ok effects ->
                                let
                                    _ =
                                        effects
                                            |> List.foldl (\eff () -> runEffect eff) ()
                                in
                                ( Done, Cmd.none )

                            Err ( error, effects ) ->
                                let
                                    _ =
                                        effects
                                            |> List.foldl (\eff () -> runEffect eff) ()
                                in
                                let
                                    errorCode : Int
                                    errorCode =
                                        if Error.isInterpreterError error then
                                            70

                                        else
                                            65
                                in
                                ( Done
                                , exitWithMessage
                                    ( errorCode
                                    , Error.toString error
                                    )
                                )

            else
                -- throwing read file contents away
                ( model, Cmd.none )

        ReplWaitingForInput ->
            ( model, Cmd.none )

        Done ->
            ( model, Cmd.none )
