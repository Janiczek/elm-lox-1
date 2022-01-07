module Parser.Internal exposing
    ( Parser
    , Step(..)
    , andThen
    , chompIf
    , chompIf_
    , loop
    , map
    , map3
    , maybe
    , oneOf
    , run
    , succeed
    )

{-| Parser working on arbitrary token types
-}

import Error exposing (Bug(..), Error, ParserError(..), Type(..))
import Token exposing (Token)


type Parser a
    = Parser (List Token -> Result (List Error) ( a, List Token ))


run : Parser a -> List Token -> Result (List Error) a
run (Parser parse) tokens =
    parse tokens
        |> Result.map Tuple.first


succeed : a -> Parser a
succeed value =
    Parser <| \tokens -> Ok ( value, tokens )


fail : Error.Type -> Parser a
fail error =
    Parser <|
        \tokens ->
            case tokens of
                t :: _ ->
                    Err [ Error.error (Token.line t) error ]

                [] ->
                    Debug.todo "Parser.Internal.fail (1) - no token to get the error line from, what to do?"


map : (a -> b) -> Parser a -> Parser b
map fn (Parser parse) =
    Parser <|
        \tokens ->
            case parse tokens of
                Err err ->
                    Err err

                Ok ( a, rest ) ->
                    Ok ( fn a, rest )


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap parserA parserFn =
    parserFn
        |> andThen (\fn -> map fn parserA)


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 fn parserA parserB =
    succeed fn
        |> andMap parserA
        |> andMap parserB


map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 fn parserA parserB parserC =
    succeed fn
        |> andMap parserA
        |> andMap parserB
        |> andMap parserC


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen fn (Parser parse) =
    Parser <|
        \tokens ->
            case parse tokens of
                Err err ->
                    Err err

                Ok ( a, rest ) ->
                    let
                        (Parser nextParse) =
                            fn a
                    in
                    nextParse rest


chompIf_ : (Token -> Bool) -> Parser (Maybe Token)
chompIf_ isTokenAllowed =
    Parser <|
        \tokens ->
            case tokens of
                [] ->
                    Ok ( Nothing, tokens )

                token :: rest ->
                    if isTokenAllowed token then
                        Ok ( Just token, rest )

                    else
                        Ok ( Nothing, tokens )


chompIf : (Token -> Bool) -> Error.Type -> Parser Token
chompIf isTokenAllowed error =
    chompIf_ isTokenAllowed
        |> andThen (maybe identity error)


{-| Reports the last parser's error if needed
-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <| oneOfHelp parsers


oneOfHelp : List (Parser a) -> List Token -> Result (List Error) ( a, List Token )
oneOfHelp parsers tokens =
    case parsers of
        [] ->
            case tokens of
                t :: _ ->
                    Err [ Error.error (Token.line t) (ParserError EmptyOneOf) ]

                [] ->
                    Debug.todo "Parser.Internal.fail (2) - no token to get the error line from, what to do?"

        (Parser parse) :: restOfParsers ->
            case parse tokens of
                Err err ->
                    if List.isEmpty restOfParsers then
                        Err err

                    else
                        oneOfHelp restOfParsers tokens

                Ok ( val, restOfTokens ) ->
                    Ok ( val, restOfTokens )


type Step state a
    = Loop state
    | Done a


loop : (state -> Parser (Step state a)) -> state -> Parser a
loop callback state =
    Parser <| loopHelp callback state


loopHelp : (state -> Parser (Step state a)) -> state -> List Token -> Result (List Error) ( a, List Token )
loopHelp callback state tokens =
    let
        (Parser parse) =
            callback state
    in
    case parse tokens of
        Err err ->
            Err err

        Ok ( Loop newState, newTokens ) ->
            loopHelp callback newState newTokens

        Ok ( Done val, newTokens ) ->
            Ok ( val, newTokens )


maybe : (a -> b) -> Error.Type -> Maybe a -> Parser b
maybe fn err maybeToken =
    maybeToken
        |> Maybe.map (fn >> succeed)
        |> Maybe.withDefault (fail err)
