module Interpreter exposing (interpretExpr, interpretProgram)

import Dict exposing (Dict)
import Effect exposing (Effect(..))
import Error exposing (Error, InterpreterError(..), Type(..))
import Expr exposing (Expr(..))
import Stmt exposing (Stmt(..))
import Token exposing (Token)
import Value exposing (Value(..))


type alias Env =
    Dict String Value


interpretProgram : List Stmt -> Result ( Error, List Effect ) (List Effect)
interpretProgram stmts_ =
    let
        go : Env -> List Effect -> List Stmt -> Result ( Error, List Effect ) (List Effect)
        go env effs stmts =
            case stmts of
                [] ->
                    Ok <| List.reverse effs

                stmt :: rest ->
                    case interpretStmt env stmt of
                        Err err ->
                            Err ( err, effs )

                        Ok res ->
                            case res.effect of
                                Nothing ->
                                    go res.env effs rest

                                Just eff ->
                                    go res.env (eff :: effs) rest
    in
    go Dict.empty [] stmts_


interpretStmt : Env -> Stmt -> Result Error { env : Env, effect : Maybe Effect }
interpretStmt env stmt =
    case stmt of
        ExprStmt expr ->
            -- TODO this will surely later return effects
            interpretExpr env expr
                |> Result.map
                    (\res ->
                        { env = res.env
                        , effect = Nothing
                        }
                    )

        Print expr ->
            interpretExpr env expr
                -- TODO if interpretExpr later returns effects too, we need to not drop them here
                |> Result.map
                    (\res ->
                        { env = res.env
                        , effect = Just (PrintEff (Value.toString res.value))
                        }
                    )

        VarDecl name maybeExpr ->
            case maybeExpr of
                Nothing ->
                    Ok
                        { env = Dict.insert name VNil env
                        , effect = Nothing
                        }

                Just expr ->
                    interpretExpr env expr
                        |> Result.map
                            (\res ->
                                { env = Dict.insert name res.value res.env
                                , effect = Nothing
                                }
                            )


interpretExpr : Env -> Expr -> Result Error { env : Env, value : Value }
interpretExpr env expr =
    {- TODO we'll likely later add effects / ... to the `value` returned record
       if not, we can simplify a lot of things here!

       When adding effects to the returned record please rename the `value` field
       so that you can check if you need to _not throw away_ some effects from
       children `interpretExpr` calls.
    -}
    case expr of
        Nil ->
            Ok { env = env, value = VNil }

        False_ ->
            Ok { env = env, value = VBool False }

        True_ ->
            Ok { env = env, value = VBool True }

        LiteralString str ->
            Ok { env = env, value = VString str }

        LiteralNumber float ->
            Ok { env = env, value = VFloat float }

        Grouping e ->
            interpretExpr env e

        Identifier name ->
            case Dict.get name env of
                Nothing ->
                    Err <|
                        Error.error
                            -1
                            (InterpreterError <| UnknownIdentifier name)

                Just value ->
                    Ok { env = env, value = value }

        Unary { operator, right } ->
            let
                tokenType : Token.Type
                tokenType =
                    Token.type_ operator
            in
            case tokenType of
                Token.Minus ->
                    interpretExpr env right
                        |> Result.andThen
                            (\r ->
                                floatNegate operator r.value
                                    |> Result.map (\newValue -> { r | value = newValue })
                            )

                Token.Bang ->
                    interpretExpr env right
                        |> Result.map (mapValue boolNegate)

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

                numOp : (Float -> Float -> Float) -> Result Error { env : Env, value : Value }
                numOp op =
                    Result.map2
                        (\l r ->
                            case ( l.value, r.value ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| setValue (VFloat (op leftFloat rightFloat)) r

                                ( _, VFloat _ ) ->
                                    -- left is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI l.value)
                                        )

                                _ ->
                                    -- right is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI r.value)
                                        )
                        )
                        (interpretExpr env left)
                        (interpretExpr env right)
                        |> Result.andThen identity

                comparisonOp : (Float -> Float -> Bool) -> Result Error { env : Env, value : Value }
                comparisonOp op =
                    Result.map2
                        (\l r ->
                            case ( l.value, r.value ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| setValue (VBool (op leftFloat rightFloat)) r

                                ( _, VFloat _ ) ->
                                    -- left is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI l.value)
                                        )

                                _ ->
                                    -- right is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI r.value)
                                        )
                        )
                        (interpretExpr env left)
                        (interpretExpr env right)
                        |> Result.andThen identity

                equalityOp : (Value -> Value -> Bool) -> Result Error { env : Env, value : Value }
                equalityOp op =
                    Result.map2 (\l r -> setValue (VBool (op l.value r.value)) r)
                        (interpretExpr env left)
                        (interpretExpr env right)
            in
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
                        (\l r ->
                            case ( l.value, r.value ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| setValue (VFloat (leftFloat + rightFloat)) r

                                ( VString leftString, VString rightString ) ->
                                    Ok <| setValue (VString (leftString ++ rightString)) r

                                ( VFloat _, _ ) ->
                                    -- right is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI r.value)
                                        )

                                ( VString _, _ ) ->
                                    -- right is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedStringI r.value)
                                        )

                                _ ->
                                    -- left is faulty
                                    Err
                                        (Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberOrString l.value)
                                        )
                        )
                        (interpretExpr env left)
                        (interpretExpr env right)
                        |> Result.andThen identity

                _ ->
                    Err
                        (Error.error
                            (Token.line operator)
                            (InterpreterError <| UnexpectedBinaryOperator tokenType)
                        )


mapValue : (a -> a) -> { r | value : a } -> { r | value : a }
mapValue fn record =
    { record | value = fn record.value }


setValue : a -> { r | value : a } -> { r | value : a }
setValue value2 record =
    { record | value = value2 }


floatNegate : Token -> Value -> Result Error Value
floatNegate operator value2 =
    case value2 of
        VFloat num ->
            Ok (VFloat (negate num))

        _ ->
            Err
                (Error.error
                    (Token.line operator)
                    (InterpreterError <| ExpectedNumberI value2)
                )


boolNegate : Value -> Value
boolNegate value2 =
    VBool (not (Value.isTruthy value2))
