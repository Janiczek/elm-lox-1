module Stmt exposing (Stmt(..))

import Expr exposing (Expr)


type Stmt
    = ExprStmt Expr
    | If
        { condition : Expr
        , then_ : Stmt
        , else_ : Maybe Stmt
        }
    | While
        { condition : Expr
        , body : Stmt
        }
    | Print Expr
    | VarDecl String (Maybe Expr)
    | Block (List Stmt)
