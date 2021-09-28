module Scanner exposing (scanTokens)

import Error exposing (Bug(..), Error, Type(..))
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
                    Ok Nothing ->
                        go newState errors tokens

                    Ok (Just newToken) ->
                        go newState errors (newToken :: tokens)

                    Err err ->
                        go newState (err :: errors) tokens
    in
    go initState [] []
        |> Result.map List.reverse
        |> Result.mapError List.reverse


{-|

    Err = something unexpected was found
    Ok Nothing = we parsed and moved forward but no token was produced. Eg.
                 line comments
    Ok (Just token) = we parsed a token and moved forward!

-}
scanToken : State -> ( Result Error (Maybe Token), State )
scanToken state =
    let
        ( currentChar, state1 ) =
            advance state

        ifMatches : String -> Token.Type -> Token.Type -> State -> ( Result Error (Maybe Token), State )
        ifMatches nextChar then_ else_ firstState =
            let
                ( matches, secondState ) =
                    match nextChar firstState
            in
            if matches then
                token then_ firstState secondState

            else
                token else_ firstState secondState

        nothing : ( Result Error (Maybe Token), State )
        nothing =
            ( Ok Nothing, state1 )
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

        "/" ->
            let
                ( matches, state2 ) =
                    match "/" state1
            in
            if matches then
                -- // line comment
                ( Ok Nothing, skipUntilNewline state2 )

            else
                token Token.Slash state1 state2

        " " ->
            nothing

        "\u{000D}" ->
            -- '\r'
            nothing

        "\t" ->
            nothing

        "\n" ->
            ( Ok Nothing, { state1 | line = state1.line + 1 } )

        "\"" ->
            string state1

        _ ->
            if isDigit currentChar then
                number state1

            else
                -- fall through
                error (UnexpectedCharacter currentChar) state state1


isDigit : String -> Bool
isDigit stringChar =
    case String.toList stringChar of
        [ char ] ->
            Char.isDigit char

        _ ->
            False


token : Token.Type -> State -> State -> ( Result Error (Maybe Token), State )
token type_ firstState secondState =
    ( Ok <|
        Just <|
            Token.token
                type_
                (String.slice firstState.start secondState.current firstState.program)
                firstState.line
    , secondState
    )


error : Error.Type -> State -> State -> ( Result Error (Maybe Token), State )
error err firstState secondState =
    ( Err <| Error.error firstState.line err
    , secondState
    )


advance : State -> ( String, State )
advance state =
    ( current state
    , { state | current = state.current + 1 }
    )


current : State -> String
current state =
    String.slice state.current (state.current + 1) state.program


next : State -> String
next state =
    String.slice (state.current + 1) (state.current + 2) state.program


{-| only advance if the next char is the one we want
-}
match : String -> State -> ( Bool, State )
match wantedChar state =
    let
        nextChar =
            current state
    in
    if isAtEnd state || nextChar /= wantedChar then
        ( False, state )

    else
        ( True, { state | current = state.current + 1 } )


isAtEnd : State -> Bool
isAtEnd state =
    state.current >= state.programLength


skipUntilNewline : State -> State
skipUntilNewline state =
    if isAtEnd state || current state == "\n" then
        state

    else
        skipUntilNewline (skipOne state)


skipOne : State -> State
skipOne state =
    { state | current = state.current + 1 }


skipWithNewlineHandlingUntil : String -> State -> State
skipWithNewlineHandlingUntil wantedChar state =
    let
        current_ =
            current state
    in
    if isAtEnd state || current_ == wantedChar then
        state

    else if current_ == "\n" then
        skipWithNewlineHandlingUntil
            wantedChar
            { state
                | line = state.line + 1
                , current = state.current + 1
            }

    else
        skipWithNewlineHandlingUntil wantedChar (skipOne state)


skipWhile : (String -> Bool) -> State -> State
skipWhile predicate state =
    -- TODO doesn't handle newlines. Is that ever a problem?
    if predicate (current state) then
        skipWhile predicate (skipOne state)

    else
        state


string : State -> ( Result Error (Maybe Token), State )
string stateAfterStartQuote =
    let
        stateAfterContents : State
        stateAfterContents =
            skipWithNewlineHandlingUntil "\"" stateAfterStartQuote
    in
    if isAtEnd stateAfterContents then
        error UnterminatedString stateAfterStartQuote stateAfterContents

    else
        let
            stateAfterEndQuote : State
            stateAfterEndQuote =
                skipOne stateAfterContents

            contents : String
            contents =
                String.slice
                    stateAfterStartQuote.current
                    stateAfterContents.current
                    stateAfterStartQuote.program
        in
        token
            (Token.String contents)
            stateAfterStartQuote
            stateAfterEndQuote


number : State -> ( Result Error (Maybe Token), State )
number stateAfterFirstNumber =
    let
        stateAfterIntegralPart : State
        stateAfterIntegralPart =
            skipWhile isDigit stateAfterFirstNumber

        stateAfterPossiblyDecimalPart : State
        stateAfterPossiblyDecimalPart =
            if
                (current stateAfterIntegralPart == ".")
                    && isDigit (next stateAfterIntegralPart)
            then
                skipWhile isDigit (skipOne stateAfterIntegralPart)

            else
                stateAfterIntegralPart

        number_ : Maybe Float
        number_ =
            String.slice
                stateAfterFirstNumber.start
                stateAfterPossiblyDecimalPart.current
                stateAfterFirstNumber.program
                |> String.toFloat
    in
    case number_ of
        Just float ->
            token
                (Token.Number float)
                stateAfterFirstNumber
                stateAfterPossiblyDecimalPart

        Nothing ->
            error
                (Bug ScannedFloatCouldntBeConvertedFromString)
                stateAfterFirstNumber
                stateAfterPossiblyDecimalPart
