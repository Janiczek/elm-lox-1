module Stmt exposing (Stmt(..))

import Expr exposing (Expr)


type Stmt
    = ExprStmt Expr
    | Print Expr
    | VarDecl String (Maybe Expr)
    | Block (List Stmt)
