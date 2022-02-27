module AstPrinter exposing (printExpr, printStatement)

import Expr exposing (Expr(..))
import Stmt exposing (Stmt(..))
import Token


printStatement : Stmt -> String
printStatement stmt =
    case stmt of
        ExprStmt expr ->
            printExpr expr ++ ";"

        If { condition, then_, else_ } ->
            "if ("
                ++ printExpr condition
                ++ ") "
                ++ printStatement then_
                ++ (case else_ of
                        Nothing ->
                            ""

                        Just else__ ->
                            " else " ++ printStatement else__
                   )

        Print expr ->
            "print " ++ printExpr expr ++ ";"

        VarDecl name maybeExpr ->
            "var "
                ++ name
                ++ (maybeExpr
                        |> Maybe.map (\e -> " = " ++ printExpr e)
                        |> Maybe.withDefault ""
                   )
                ++ ";"

        Block stmts ->
            "{\\n"
                ++ (stmts
                        |> List.map (printStatement >> indent)
                        |> String.join "\n"
                   )
                ++ "\\n}"


indent : String -> String
indent str =
    "  " ++ str


printExpr : Expr -> String
printExpr expr =
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

        LogicOr left right ->
            parenthesize
                "or"
                [ left, right ]

        LogicAnd left right ->
            parenthesize
                "and"
                [ left, right ]


parenthesize : String -> List Expr -> String
parenthesize head exprs =
    "("
        ++ ((head :: List.map printExpr exprs)
                |> String.join " "
           )
        ++ ")"
