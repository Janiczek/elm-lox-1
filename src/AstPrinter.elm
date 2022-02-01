module AstPrinter exposing (print)

import Expr exposing (Expr(..))
import Token


print : Expr -> String
print expr =
    case expr of
        Binary { left, operator, right } ->
            parenthesize
                (Token.lexeme operator)
                [ left
                , right
                ]

        Unary { operator, right } ->
            parenthesize
                (Token.lexeme operator)
                [ right ]

        Grouping inner ->
            parenthesize
                "group"
                [ inner ]

        LiteralString inner ->
            inner

        LiteralNumber inner ->
            String.fromFloat inner

        Identifier identifier ->
            identifier

        False_ ->
            "false"

        True_ ->
            "true"

        Nil ->
            "nil"

        Assign { names, value } ->
            parenthesize
                "="
                (List.map Identifier names ++ [ value ])


parenthesize : String -> List Expr -> String
parenthesize head exprs =
    "("
        ++ ((head :: List.map print exprs)
                |> String.join " "
           )
        ++ ")"
