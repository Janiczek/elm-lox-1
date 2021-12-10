module Expr exposing (Expr(..))

import Token exposing (Token)


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
    | Unary
        { operator : Token
        , right : Expr
        }
