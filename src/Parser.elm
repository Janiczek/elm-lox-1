module Parser exposing (parseExpr)

import Expr exposing (Expr(..))
import Parser.Internal as Parser exposing (Parser)
import Token exposing (Token, Type(..))


type alias LoxParser a =
    Parser ParseError Token a


type ParseError
    = ExpectedToken Token.Type
    | ExpectedNumber
    | ExpectedString


parseExpr : List Token -> Result (Parser.Error ParseError) Expr
parseExpr tokens =
    Parser.run expression tokens


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
        |> Parser.andThen (Parser.loop (leftAssociativeBinopHelp isTokenAllowed))


leftAssociativeBinopHelp :
    (Token -> Bool)
    -> Expr
    -> LoxParser (Parser.Step Expr Expr)
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


primary : LoxParser Expr
primary =
    Parser.oneOf
        [ Parser.chompIf
            (isSimpleToken [ Token.False_ ])
            (ExpectedToken Token.False_)
            |> Parser.map (\_ -> Expr.False_)
        , Parser.chompIf
            (isSimpleToken [ Token.True_ ])
            (ExpectedToken Token.True_)
            |> Parser.map (\_ -> Expr.True_)
        , Parser.chompIf
            (isSimpleToken [ Token.Nil ])
            (ExpectedToken Token.Nil)
            |> Parser.map (\_ -> Expr.Nil)
        , Parser.chompIf Token.isNumber ExpectedNumber
            |> Parser.map Token.getNumber
            |> Parser.andThen (Parser.maybe LiteralNumber ExpectedNumber)
        , Parser.chompIf Token.isString ExpectedString
            |> Parser.map Token.getString
            |> Parser.andThen (Parser.maybe LiteralString ExpectedString)
        , Parser.map3
            (\_ expr _ -> Grouping expr)
            (Parser.chompIf
                (isSimpleToken [ LeftParen ])
                (ExpectedToken LeftParen)
            )
            expression
            (Parser.chompIf
                (isSimpleToken [ RightParen ])
                (ExpectedToken RightParen)
            )
        ]
