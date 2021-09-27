module Scanner exposing (scanTokens)

import Error exposing (Error, Type(..))
import Token exposing (Token, Type(..))


type alias State =
    { program : String
    , programLength : Int
    , start : Int
    , current : Int
    , line : Int
    }


scanTokens : String -> Result (List Error) (List Token)
scanTokens program =
    let
        initState : State
        initState =
            { program = program
            , programLength = String.length program
            , start = 0
            , current = 0
            , line = 1
            }

        go : State -> List Error -> List Token -> Result (List Error) (List Token)
        go state errors tokens =
            if isAtEnd state then
                if List.isEmpty errors then
                    Ok <| Token.token EOF "" state.line :: tokens

                else
                    Err errors

            else
                let
                    ( result, newState ) =
                        scanToken { state | start = state.current }
                in
                case result of
                    Ok newToken ->
                        go newState errors (newToken :: tokens)

                    Err err ->
                        go newState (err :: errors) tokens
    in
    go initState [] []
        |> Result.map List.reverse
        |> Result.mapError List.reverse


scanToken : State -> ( Result Error Token, State )
scanToken state =
    let
        ( currentChar, newState ) =
            advance state

        token : Token.Type -> ( Result Error Token, State )
        token type_ =
            ( Ok
                (Token.token
                    type_
                    (String.slice state.start newState.current state.program)
                    state.line
                )
            , newState
            )

        error : Error.Type -> ( Result Error Token, State )
        error err =
            ( Err <| Error.error state.line err
            , newState
            )
    in
    case currentChar of
        "(" ->
            token LeftParen

        ")" ->
            token RightParen

        "{" ->
            token LeftBrace

        "}" ->
            token RightBrace

        "," ->
            token Comma

        "." ->
            token Dot

        "-" ->
            token Minus

        "+" ->
            token Plus

        ";" ->
            token Semicolon

        "*" ->
            token Star

        _ ->
            error <| UnexpectedCharacter currentChar


advance : State -> ( String, State )
advance state =
    let
        newCurrent =
            state.current + 1
    in
    ( String.slice state.current newCurrent state.program
    , { state | current = newCurrent }
    )


isAtEnd : State -> Bool
isAtEnd state =
    state.current >= state.programLength
