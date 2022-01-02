module Parser.Internal exposing
    ( Error(..)
    , Parser
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

-- Parser working on arbitrary token types


type Parser e t a
    = Parser (List t -> Result (Error e) ( a, List t ))


type Error e
    = EmptyOneOf
    | CustomError e


run : Parser e t a -> List t -> Result (Error e) a
run (Parser parse) tokens =
    parse tokens
        |> Result.map Tuple.first


succeed : a -> Parser e t a
succeed value =
    Parser <| \tokens -> Ok ( value, tokens )


fail : e -> Parser e t a
fail error =
    Parser <| \_ -> Err (CustomError error)


map : (a -> b) -> Parser e t a -> Parser e t b
map fn (Parser parse) =
    Parser <|
        \tokens ->
            case parse tokens of
                Err err ->
                    Err err

                Ok ( a, rest ) ->
                    Ok ( fn a, rest )


andMap : Parser e t a -> Parser e t (a -> b) -> Parser e t b
andMap parserA parserFn =
    parserFn
        |> andThen (\fn -> map fn parserA)


map2 : (a -> b -> c) -> Parser e t a -> Parser e t b -> Parser e t c
map2 fn parserA parserB =
    succeed fn
        |> andMap parserA
        |> andMap parserB


map3 : (a -> b -> c -> d) -> Parser e t a -> Parser e t b -> Parser e t c -> Parser e t d
map3 fn parserA parserB parserC =
    succeed fn
        |> andMap parserA
        |> andMap parserB
        |> andMap parserC


andThen : (a -> Parser e t b) -> Parser e t a -> Parser e t b
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


chompIf_ : (t -> Bool) -> Parser e t (Maybe t)
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


chompIf : (t -> Bool) -> e -> Parser e t t
chompIf isTokenAllowed error =
    chompIf_ isTokenAllowed
        |> andThen (maybe identity error)


{-| Reports the last parser's error if needed
-}
oneOf : List (Parser e t a) -> Parser e t a
oneOf parsers =
    Parser <| oneOfHelp parsers


oneOfHelp : List (Parser e t a) -> List t -> Result (Error e) ( a, List t )
oneOfHelp parsers tokens =
    case parsers of
        [] ->
            Err EmptyOneOf

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


loop : (state -> Parser e t (Step state a)) -> state -> Parser e t a
loop callback state =
    Parser <| loopHelp callback state


loopHelp : (state -> Parser e t (Step state a)) -> state -> List t -> Result (Error e) ( a, List t )
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


maybe : (a -> b) -> e -> Maybe a -> Parser e t b
maybe fn err maybeToken =
    maybeToken
        |> Maybe.map (fn >> succeed)
        |> Maybe.withDefault (fail err)
