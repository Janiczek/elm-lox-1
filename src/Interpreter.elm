module Interpreter exposing (interpretExpr, interpretProgram)

import Effect exposing (Effect(..))
import Error exposing (Error, InterpreterError(..), Type(..))
import Expr exposing (Expr(..))
import Stmt exposing (Stmt(..))
import Token exposing (Token)
import Value exposing (Value(..))


interpretProgram : List Stmt -> Result ( Error, List Effect ) (List Effect)
interpretProgram stmts_ =
    let
        go effs stmts =
            case stmts of
                [] ->
                    Ok <| List.reverse effs

                stmt :: rest ->
                    case interpretStmt stmt of
                        Err err ->
                            Err ( err, effs )

                        Ok Nothing ->
                            go effs rest

                        Ok (Just eff) ->
                            go (eff :: effs) rest
    in
    go [] stmts_


interpretStmt : Stmt -> Result Error (Maybe Effect)
interpretStmt stmt =
    case stmt of
        ExprStmt expr ->
            interpretExpr expr
                |> Result.map (\_ -> Nothing)

        Print expr ->
            interpretExpr expr
                -- TODO if interpretExpr later returns effects too, we need to not drop them here
                |> Result.map (.value >> Value.toString >> PrintEff >> Just)


interpretExpr : Expr -> Result Error { value : Value }
interpretExpr expr =
    {- TODO we'll likely later add effects / ... to the `value` returned record
       if not, we can simplify a lot of things here!

       When adding effects to the returned record please rename the `value` field
       so that you can check if you need to _not throw away_ some effects from
       children `interpretExpr` calls.
    -}
    let
        toRec : Value -> { value : Value }
        toRec value =
            { value = value }
    in
    case expr of
        Nil ->
            Ok <| toRec VNil

        False_ ->
            Ok <| toRec <| VBool False

        True_ ->
            Ok <| toRec <| VBool True

        LiteralString str ->
            Ok <| toRec <| VString str

        LiteralNumber float ->
            Ok <| toRec <| VFloat float

        Grouping e ->
            interpretExpr e

        Identifier _ ->
            Debug.todo "interpret identifier"

        Unary { operator, right } ->
            let
                tokenType : Token.Type
                tokenType =
                    Token.type_ operator
            in
            case tokenType of
                Token.Minus ->
                    interpretExpr right
                        |> Result.andThen (.value >> floatNegate operator)
                        |> Result.map toRec

                Token.Bang ->
                    interpretExpr right
                        |> Result.map (.value >> boolNegate >> toRec)

                _ ->
                    Err
                        (Error.error
                            (Token.line operator)
                            (InterpreterError <| UnexpectedUnaryOperator tokenType)
                        )

        Binary { left, operator, right } ->
            let
                tokenType : Token.Type
                tokenType =
                    Token.type_ operator

                numOp : (Float -> Float -> Float) -> Result Error Value
                numOp op =
                    Result.map2
                        (\leftExpr rightExpr ->
                            case ( leftExpr.value, rightExpr.value ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| VFloat (op leftFloat rightFloat)

                                ( _, VFloat _ ) ->
                                    -- left is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI leftExpr.value)
                                        )

                                _ ->
                                    -- right is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI rightExpr.value)
                                        )
                        )
                        (interpretExpr left)
                        (interpretExpr right)
                        |> Result.andThen identity

                comparisonOp : (Float -> Float -> Bool) -> Result Error Value
                comparisonOp op =
                    Result.map2
                        (\leftExpr rightExpr ->
                            case ( leftExpr.value, rightExpr.value ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| VBool (op leftFloat rightFloat)

                                ( _, VFloat _ ) ->
                                    -- left is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI leftExpr.value)
                                        )

                                _ ->
                                    -- right is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI rightExpr.value)
                                        )
                        )
                        (interpretExpr left)
                        (interpretExpr right)
                        |> Result.andThen identity

                equalityOp : (Value -> Value -> Bool) -> Result Error Value
                equalityOp op =
                    Result.map2 (\leftExpr rightExpr -> op leftExpr.value rightExpr.value)
                        (interpretExpr left)
                        (interpretExpr right)
                        |> Result.map VBool
            in
            Result.map toRec <|
                case tokenType of
                    Token.Minus ->
                        numOp (-)

                    Token.Slash ->
                        numOp (/)

                    Token.Star ->
                        numOp (*)

                    Token.Greater ->
                        comparisonOp (>)

                    Token.GreaterEqual ->
                        comparisonOp (>=)

                    Token.Less ->
                        comparisonOp (<)

                    Token.LessEqual ->
                        comparisonOp (<=)

                    Token.BangEqual ->
                        equalityOp (/=)

                    Token.EqualEqual ->
                        equalityOp (==)

                    Token.Plus ->
                        -- Both (Float,Float) and (String,String) is valid
                        Result.map2
                            (\leftExpr rightExpr ->
                                case ( leftExpr.value, rightExpr.value ) of
                                    ( VFloat leftFloat, VFloat rightFloat ) ->
                                        Ok <| VFloat (leftFloat + rightFloat)

                                    ( VString leftString, VString rightString ) ->
                                        Ok <| VString (leftString ++ rightString)

                                    ( VFloat _, _ ) ->
                                        -- right is faulty
                                        Err
                                            (Error.error
                                                (Token.line operator)
                                                (InterpreterError <| ExpectedNumberI rightExpr.value)
                                            )

                                    ( VString _, _ ) ->
                                        -- right is faulty
                                        Err
                                            (Error.error
                                                (Token.line operator)
                                                (InterpreterError <| ExpectedStringI rightExpr.value)
                                            )

                                    _ ->
                                        -- left is faulty
                                        Err
                                            (Error.error
                                                (Token.line operator)
                                                (InterpreterError <| ExpectedNumberOrString leftExpr.value)
                                            )
                            )
                            (interpretExpr left)
                            (interpretExpr right)
                            |> Result.andThen identity

                    _ ->
                        Err
                            (Error.error
                                (Token.line operator)
                                (InterpreterError <| UnexpectedBinaryOperator tokenType)
                            )


floatNegate : Token -> Value -> Result Error Value
floatNegate operator value =
    case value of
        VFloat num ->
            Ok (VFloat (negate num))

        _ ->
            Err
                (Error.error
                    (Token.line operator)
                    (InterpreterError <| ExpectedNumberI value)
                )


boolNegate : Value -> Value
boolNegate value =
    VBool (not (Value.isTruthy value))
