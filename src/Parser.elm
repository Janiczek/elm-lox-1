module Parser exposing (parseExpr)

import Error exposing (ParserError(..), Type(..))
import Expr exposing (Expr(..))
import Parser.Internal as Parser exposing (Parser)
import Token exposing (Token, Type(..))


parseExpr : List Token -> Result (List Error.Error) Expr
parseExpr tokens =
    Parser.run expression tokens


expression : Parser Expr
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
    Parser Expr
    -> (Token -> Bool)
    -> Parser Expr
leftAssociativeBinop exprParser isTokenAllowed =
    exprParser
        |> Parser.andThen (Parser.loop (leftAssociativeBinopHelp isTokenAllowed))


leftAssociativeBinopHelp :
    (Token -> Bool)
    -> Expr
    -> Parser (Parser.Step Expr Expr)
leftAssociativeBinopHelp isTokenAllowed leftExpr =
    Parser.chompIf_ isTokenAllowed
        |> Parser.andThen
            (\maybeOperator ->
                case maybeOperator of
                    Nothing ->
                        Parser.succeed (Parser.Done leftExpr)

                    Just operator ->
                        comparison
                            |> Parser.map
                                (\right ->
                                    Parser.Loop (binary leftExpr operator right)
                                )
            )


equality : Parser Expr
equality =
    leftAssociativeBinop
        comparison
        (isSimpleToken [ BangEqual, EqualEqual ])


comparison : Parser Expr
comparison =
    leftAssociativeBinop
        term
        (isSimpleToken [ Greater, GreaterEqual, Less, LessEqual ])


term : Parser Expr
term =
    leftAssociativeBinop
        factor
        (isSimpleToken [ Minus, Plus ])


factor : Parser Expr
factor =
    leftAssociativeBinop
        unary
        (isSimpleToken [ Slash, Star ])


unary : Parser Expr
unary =
    Parser.chompIf_ (isSimpleToken [ Bang, Minus ])
        |> Parser.andThen
            (\maybeOperator ->
                case maybeOperator of
                    Just operator ->
                        unary
                            |> Parser.map
                                (\right ->
                                    Unary
                                        { operator = operator
                                        , right = right
                                        }
                                )

                    Nothing ->
                        primary
            )


primary : Parser Expr
primary =
    Parser.oneOf
        [ Parser.chompIf
            (isSimpleToken [ Token.False_ ])
            (ParserError (ExpectedToken Token.False_))
            |> Parser.map (\_ -> Expr.False_)
        , Parser.chompIf
            (isSimpleToken [ Token.True_ ])
            (ParserError (ExpectedToken Token.True_))
            |> Parser.map (\_ -> Expr.True_)
        , Parser.chompIf
            (isSimpleToken [ Token.Nil ])
            (ParserError (ExpectedToken Token.Nil))
            |> Parser.map (\_ -> Expr.Nil)
        , Parser.chompIf Token.isNumber (ParserError ExpectedNumberP)
            |> Parser.map Token.getNumber
            |> Parser.andThen (Parser.maybe LiteralNumber (ParserError ExpectedNumberP))
        , Parser.chompIf Token.isString (ParserError ExpectedStringP)
            |> Parser.map Token.getString
            |> Parser.andThen (Parser.maybe LiteralString (ParserError ExpectedStringP))
        , Parser.map3
            (\_ expr _ -> Grouping expr)
            (Parser.chompIf
                (isSimpleToken [ LeftParen ])
                (ParserError (ExpectedToken LeftParen))
            )
            expression
            (Parser.chompIf
                (isSimpleToken [ RightParen ])
                (ParserError (ExpectedToken RightParen))
            )
        ]
