module Scanner exposing (scan)

import Dict exposing (Dict)
import Error exposing (Bug(..), Error, ScannerError(..), Type(..))
import Token exposing (Token, Type(..))


type alias State =
    { program : String
    , programLength : Int
    , start : Int
    , current : Int
    , line : Int
    }


scan : String -> Result Error (List Token)
scan program =
    let
        initState : State
        initState =
            { program = program
            , programLength = String.length program
            , start = 0
            , current = 0
            , line = 1
            }

        go : State -> List Token -> Result Error (List Token)
        go state tokens =
            if isAtEnd state then
                Ok tokens

            else
                let
                    ( result, newState ) =
                        scanToken { state | start = state.current }
                in
                case result of
                    Ok Nothing ->
                        go newState tokens

                    Ok (Just newToken) ->
                        go newState (newToken :: tokens)

                    Err err ->
                        Err err
    in
    go initState []
        |> Result.map List.reverse


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

        ifMatches : String -> Type -> Type -> State -> ( Result Error (Maybe Token), State )
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
            token LeftParen state state1

        ")" ->
            token RightParen state state1

        "{" ->
            token LeftBrace state state1

        "}" ->
            token RightBrace state state1

        "," ->
            token Comma state state1

        "." ->
            token Dot state state1

        "-" ->
            token Minus state state1

        "+" ->
            token Plus state state1

        ";" ->
            token Semicolon state state1

        "*" ->
            token Star state state1

        "!" ->
            ifMatches "=" BangEqual Bang state1

        "=" ->
            ifMatches "=" EqualEqual Equal state1

        "<" ->
            ifMatches "=" LessEqual Less state1

        ">" ->
            ifMatches "=" GreaterEqual Greater state1

        "/" ->
            let
                ( matches, state2 ) =
                    match "/" state1
            in
            if matches then
                -- // line comment
                ( Ok Nothing, skipUntilNewline state2 )

            else
                token Slash state1 state2

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

            else if isAlpha currentChar then
                identifier state1

            else
                -- fall through
                error (ScannerError <| UnexpectedCharacter currentChar) state state1


isDigit : String -> Bool
isDigit =
    liftToString Char.isDigit


isAlpha : String -> Bool
isAlpha =
    liftToString Char.isAlpha


isAlphaNum : String -> Bool
isAlphaNum =
    liftToString Char.isAlphaNum


liftToString : (Char -> Bool) -> String -> Bool
liftToString charPred stringChar =
    case String.toList stringChar of
        [ char ] ->
            charPred char

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
        error (ScannerError UnterminatedString) stateAfterStartQuote stateAfterContents

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
            (String contents)
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
                (Number float)
                stateAfterFirstNumber
                stateAfterPossiblyDecimalPart

        Nothing ->
            error
                (Bug ScannedFloatCouldntBeConvertedFromString)
                stateAfterFirstNumber
                stateAfterPossiblyDecimalPart


reservedWords : Dict String Type
reservedWords =
    [ ( "and", And )
    , ( "class", Class )
    , ( "else", Else )
    , ( "false", False_ )
    , ( "for", For )
    , ( "fun", Fun )
    , ( "if", If )
    , ( "nil", Nil )
    , ( "or", Or )
    , ( "print", Print )
    , ( "return", Return )
    , ( "super", Super )
    , ( "this", This )
    , ( "true", True_ )
    , ( "var", Var )
    , ( "while", While )
    ]
        |> Dict.fromList


identifier : State -> ( Result Error (Maybe Token), State )
identifier stateAfterFirstLetter =
    let
        stateAfterIdentifierChars =
            skipWhile isAlphaNum stateAfterFirstLetter

        text : String
        text =
            String.slice
                stateAfterFirstLetter.start
                stateAfterIdentifierChars.current
                stateAfterFirstLetter.program

        type_ : Type
        type_ =
            Dict.get text reservedWords
                |> Maybe.withDefault (Identifier text)
    in
    token type_ stateAfterFirstLetter stateAfterIdentifierChars
