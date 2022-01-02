module Expr exposing (Expr(..), expression, run)

import Token exposing (Token, Type(..))


type Expr
    = Binary
        { left : Expr
        , operator : Token
        , right : Expr
        }
    | Grouping Expr
    | Identifier String
    | LiteralString String
    | LiteralNumber Float
    | False_
    | True_
    | Nil
    | Unary
        { operator : Token
        , right : Expr
        }



-- Recursive descent parser?


type alias LoxParser a =
    Parser ParseError Token a


type ParseError
    = ExpectedToken Token.Type
    | ExpectedNumber
    | ExpectedString


expression : LoxParser Expr
expression =
    equality


isSimpleToken : List Token.Type -> Token -> Bool
isSimpleToken allowedTokenTypes token =
    List.member (Token.type_ token) allowedTokenTypes


binary : Expr -> Token -> Expr -> Expr
binary left operator right =
    Binary
        { left = left
        , operator = operator
        , right = right
        }


leftAssociativeBinop :
    LoxParser Expr
    -> (Token -> Bool)
    -> LoxParser Expr
leftAssociativeBinop exprParser isTokenAllowed =
    exprParser
        |> andThen (loop (leftAssociativeBinopHelp isTokenAllowed))


leftAssociativeBinopHelp :
    (Token -> Bool)
    -> Expr
    -> LoxParser (Step Expr Expr)
leftAssociativeBinopHelp isTokenAllowed leftExpr =
    chompIf_ isTokenAllowed
        |> andThen
            (\maybeOperator ->
                case maybeOperator of
                    Nothing ->
                        succeed (Done leftExpr)

                    Just operator ->
                        comparison
                            |> map (\right -> Loop (binary leftExpr operator right))
            )


equality : LoxParser Expr
equality =
    leftAssociativeBinop
        comparison
        (isSimpleToken [ BangEqual, EqualEqual ])


comparison : LoxParser Expr
comparison =
    leftAssociativeBinop
        term
        (isSimpleToken [ Greater, GreaterEqual, Less, LessEqual ])


term : LoxParser Expr
term =
    leftAssociativeBinop
        factor
        (isSimpleToken [ Minus, Plus ])


factor : LoxParser Expr
factor =
    leftAssociativeBinop
        unary
        (isSimpleToken [ Slash, Star ])


unary : LoxParser Expr
unary =
    chompIf_ (isSimpleToken [ Bang, Minus ])
        |> andThen
            (\maybeOperator ->
                case maybeOperator of
                    Just operator ->
                        unary
                            |> map
                                (\right ->
                                    Unary
                                        { operator = operator
                                        , right = right
                                        }
                                )

                    Nothing ->
                        primary
            )


primary : LoxParser Expr
primary =
    oneOf
        [ chompIf (isSimpleToken [ Token.False_ ]) (ExpectedToken Token.False_)
            |> map (\_ -> False_)
        , chompIf (isSimpleToken [ Token.True_ ]) (ExpectedToken Token.True_)
            |> map (\_ -> True_)
        , chompIf (isSimpleToken [ Token.Nil ]) (ExpectedToken Token.Nil)
            |> map (\_ -> Nil)
        , chompIf Token.isNumber ExpectedNumber
            |> map Token.getNumber
            |> andThen (maybe LiteralNumber ExpectedNumber)
        , chompIf Token.isString ExpectedString
            |> map Token.getString
            |> andThen (maybe LiteralString ExpectedString)
        , map3
            (\_ expr _ -> Grouping expr)
            (chompIf (isSimpleToken [ LeftParen ]) (ExpectedToken LeftParen))
            expression
            (chompIf (isSimpleToken [ RightParen ]) (ExpectedToken RightParen))
        ]



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
