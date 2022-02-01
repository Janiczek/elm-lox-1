module Parser exposing (parseExpr, parseProgram)

import Error exposing (Error, ParserError(..), Type(..))
import Expr exposing (Expr(..))
import Parser.Internal as Parser exposing (Parser)
import Stmt exposing (Stmt)
import Token exposing (Token, Type(..))


parseProgram : List Token -> Result Error (List Stmt)
parseProgram tokens =
    Parser.run program tokens


parseExpr : List Token -> Result Error Expr
parseExpr tokens =
    Parser.run expression tokens



-- PROGRAM


program : Parser (List Stmt)
program =
    Parser.succeed identity
        |> Parser.keep (Parser.many declaration)
        |> Parser.skip Parser.end



-- DECLARATION


declaration : Parser Stmt
declaration =
    Parser.oneOf
        [ varDeclaration
        , statement
        ]


varDeclaration : Parser Stmt
varDeclaration =
    Parser.oneOf
        [ Parser.succeed Stmt.VarDecl
            |> Parser.skip (Parser.token Token.Var)
            |> Parser.keep identifier
            |> Parser.skip (Parser.token Token.Equal)
            |> Parser.keep (Parser.map Just expression)
            |> Parser.skip (Parser.token Token.Semicolon)
        , Parser.succeed Stmt.VarDecl
            |> Parser.skip (Parser.token Token.Var)
            |> Parser.keep identifier
            |> Parser.keep (Parser.succeed Nothing)
            |> Parser.skip (Parser.token Token.Semicolon)
        ]


identifier : Parser String
identifier =
    Parser.chompIf Token.isIdentifier (ParserError ExpectedIdentifier)
        |> Parser.map Token.getIdentifier
        |> Parser.andThen (Parser.maybe identity (ParserError ExpectedIdentifier))



-- STATEMENT


statement : Parser Stmt
statement =
    Parser.oneOf
        [ printStatement
        , exprStatement
        ]


printStatement : Parser Stmt
printStatement =
    Parser.succeed Stmt.Print
        |> Parser.skip (Parser.token Token.Print)
        |> Parser.keep expression
        |> Parser.skip (Parser.token Token.Semicolon)


exprStatement : Parser Stmt
exprStatement =
    Parser.succeed Stmt.ExprStmt
        |> Parser.keep expression
        |> Parser.skip (Parser.token Token.Semicolon)



-- EXPRESSION


expression : Parser Expr
expression =
    assignment


assignment : Parser Expr
assignment =
    Parser.oneOf
        [ Parser.succeed
            (\( name, names ) value ->
                Assign
                    { names = name :: names
                    , value = value
                    }
            )
            |> Parser.keep
                (Parser.many1
                    (Parser.succeed identity
                        |> Parser.keep identifier
                        |> Parser.skip (Parser.token Token.Equal)
                    )
                )
            |> Parser.keep (Parser.lazy (\() -> assignment))
        , equality
        ]


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
        [ Parser.token Token.False_
            |> Parser.map (\_ -> Expr.False_)
        , Parser.token Token.True_
            |> Parser.map (\_ -> Expr.True_)
        , Parser.token Token.Nil
            |> Parser.map (\_ -> Expr.Nil)
        , Parser.chompIf Token.isNumber (ParserError ExpectedNumberP)
            |> Parser.map Token.getNumber
            |> Parser.andThen (Parser.maybe LiteralNumber (ParserError ExpectedNumberP))
        , Parser.chompIf Token.isString (ParserError ExpectedStringP)
            |> Parser.map Token.getString
            |> Parser.andThen (Parser.maybe LiteralString (ParserError ExpectedStringP))
        , Parser.succeed Grouping
            |> Parser.skip (Parser.token LeftParen)
            |> Parser.keep expression
            |> Parser.skip (Parser.token RightParen)
        , Parser.map Expr.Identifier identifier
        ]
