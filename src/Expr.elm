module Expr exposing (Expr(..))

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
