module Parser.Internal exposing
    ( Parser
    , Step(..)
    , andThen
    , chompIf
    , chompIf_
    , end
    , keep
    , loop
    , many
    , map
    , map3
    , maybe
    , oneOf
    , run
    , skip
    , succeed
    , token
    )

{-| Parser working on arbitrary token types
-}

import Error exposing (Bug(..), Error, ParserError(..), Type(..))
import Token exposing (Token)


type Parser a
    = Parser (List Token -> Result Error ( a, List Token ))


run : Parser a -> List Token -> Result Error a
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
            let
                line : Int
                line =
                    case tokens of
                        t :: _ ->
                            Token.line t

                        [] ->
                            -1
            in
            Err (Error.error line error)


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

                t :: rest ->
                    if isTokenAllowed t then
                        Ok ( Just t, rest )

                    else
                        Ok ( Nothing, tokens )


chompIf : (Token -> Bool) -> Error.Type -> Parser Token
chompIf isTokenAllowed error =
    chompIf_ isTokenAllowed
        |> andThen (maybe identity error)


token : Token.Type -> Parser Token
token wantedToken =
    Parser <|
        \tokens ->
            case tokens of
                [] ->
                    Err (Error.error -1 (ParserError (ExpectedToken wantedToken)))

                t :: ts ->
                    if Token.type_ t == wantedToken then
                        Ok ( t, ts )

                    else
                        Err (Error.error (Token.line t) (ParserError (ExpectedToken wantedToken)))


{-| Reports the last parser's error if needed
-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <| oneOfHelp parsers


oneOfHelp : List (Parser a) -> List Token -> Result Error ( a, List Token )
oneOfHelp parsers tokens =
    case parsers of
        [] ->
            let
                line =
                    case tokens of
                        t :: _ ->
                            Token.line t

                        [] ->
                            -1
            in
            Err (Error.error line (ParserError EmptyOneOf))

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


loopHelp : (state -> Parser (Step state a)) -> state -> List Token -> Result Error ( a, List Token )
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


end : Parser ()
end =
    Parser <|
        \tokens ->
            if List.isEmpty tokens then
                Ok ( (), tokens )

            else
                Err (Error.error -1 (ParserError ExpectedEOF))


many : Parser a -> Parser (List a)
many innerParser =
    let
        manyHelp : List a -> Parser (Step (List a) (List a))
        manyHelp state =
            oneOf
                [ innerParser
                    |> map (\inner -> Loop (inner :: state))
                , succeed (Done (List.reverse state))
                ]
    in
    loop manyHelp []


keep : Parser a -> Parser (a -> b) -> Parser b
keep =
    andMap


skip : Parser a -> Parser b -> Parser b
skip next original =
    map2 (\o _ -> o) original next
