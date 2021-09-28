module Scanner exposing (scanTokens)

import Error exposing (Error, Type(..))
import Token exposing (Token)


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
                    Ok <| Token.token Token.EOF "" state.line :: tokens

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
        ( currentChar, state1 ) =
            advance state

        token : Token.Type -> State -> State -> ( Result Error Token, State )
        token type_ firstState secondState =
            ( Ok
                (Token.token
                    type_
                    (String.slice firstState.start secondState.current firstState.program)
                    firstState.line
                )
            , secondState
            )

        error : Error.Type -> State -> State -> ( Result Error Token, State )
        error err firstState secondState =
            ( Err <| Error.error firstState.line err
            , secondState
            )

        ifMatches : String -> Token.Type -> Token.Type -> State -> ( Result Error Token, State )
        ifMatches nextChar then_ else_ firstState =
            let
                ( matches, secondState ) =
                    match nextChar firstState
            in
            if matches then
                token then_ firstState secondState

            else
                token else_ firstState secondState
    in
    case currentChar of
        "(" ->
            token Token.LeftParen state state1

        ")" ->
            token Token.RightParen state state1

        "{" ->
            token Token.LeftBrace state state1

        "}" ->
            token Token.RightBrace state state1

        "," ->
            token Token.Comma state state1

        "." ->
            token Token.Dot state state1

        "-" ->
            token Token.Minus state state1

        "+" ->
            token Token.Plus state state1

        ";" ->
            token Token.Semicolon state state1

        "*" ->
            token Token.Star state state1

        "!" ->
            ifMatches "=" Token.BangEqual Token.Bang state1

        "=" ->
            ifMatches "=" Token.EqualEqual Token.Equal state1

        "<" ->
            ifMatches "=" Token.LessEqual Token.Less state1

        ">" ->
            ifMatches "=" Token.GreaterEqual Token.Greater state1

        _ ->
            error (UnexpectedCharacter currentChar) state state1


advance : State -> ( String, State )
advance state =
    let
        newCurrent =
            state.current + 1
    in
    ( String.slice state.current newCurrent state.program
    , { state | current = newCurrent }
    )


{-| only advance if the next char is the one we want
-}
match : String -> State -> ( Bool, State )
match wantedChar state =
    let
        possiblyNewCurrent =
            state.current + 1

        nextChar =
            String.slice state.current possiblyNewCurrent state.program
    in
    if isAtEnd state || nextChar /= wantedChar then
        ( False, state )

    else
        ( True, { state | current = possiblyNewCurrent } )


isAtEnd : State -> Bool
isAtEnd state =
    state.current >= state.programLength
