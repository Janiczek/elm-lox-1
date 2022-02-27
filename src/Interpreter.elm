module Interpreter exposing (interpretExpr, interpretProgram)

import Dict exposing (Dict)
import Effect exposing (Effect(..))
import Env exposing (EnvChain)
import Error exposing (Error, InterpreterError(..), Type(..))
import Expr exposing (Expr(..))
import Result.Extra
import Stmt exposing (Stmt(..))
import Token exposing (Token)
import Value exposing (Value(..))


interpretProgram : List Stmt -> Result ( Error, List Effect ) (List Effect)
interpretProgram stmts_ =
    let
        go : EnvChain -> List Effect -> List Stmt -> Result ( Error, List Effect ) (List Effect)
        go envChain effs stmts =
            case stmts of
                [] ->
                    Ok (List.reverse effs)

                stmt :: rest ->
                    case interpretStmt envChain stmt of
                        Err ( err, stmtEffs ) ->
                            Err ( err, List.reverse (stmtEffs ++ effs) )

                        Ok res ->
                            go res.envChain (res.effects ++ effs) rest
    in
    go Env.empty [] stmts_


interpretStmt : EnvChain -> Stmt -> Result ( Error, List Effect ) { envChain : EnvChain, effects : List Effect }
interpretStmt envChain stmt =
    case stmt of
        ExprStmt expr ->
            -- TODO: likely we'll need to handle effects here
            interpretExpr envChain expr
                |> Result.mapError (\err -> ( err, [] ))
                |> Result.map
                    (\res ->
                        { envChain = res.envChain
                        , effects = []
                        }
                    )

        If { condition, then_, else_ } ->
            -- TODO: likely we'll need to handle effects here
            case interpretExpr envChain condition of
                Err err ->
                    Err ( err, [] )

                Ok conditionResult ->
                    if Value.isTruthy conditionResult.value then
                        interpretStmt conditionResult.envChain then_

                    else
                        case else_ of
                            Nothing ->
                                Ok
                                    { envChain = conditionResult.envChain
                                    , effects = []
                                    }

                            Just else__ ->
                                interpretStmt conditionResult.envChain else__

        While { condition, body } ->
            let
                -- emulating a while loop here
                go : { envChain : EnvChain, effects : List Effect } -> Result ( Error, List Effect ) { envChain : EnvChain, effects : List Effect }
                go acc =
                    -- TODO: likely we'll need to handle effects here
                    case interpretExpr acc.envChain condition of
                        Err err ->
                            Err ( err, acc.effects )

                        Ok conditionResult ->
                            if Value.isTruthy conditionResult.value then
                                case interpretStmt conditionResult.envChain body of
                                    Err err ->
                                        Err err

                                    Ok bodyResult ->
                                        go
                                            { envChain = bodyResult.envChain
                                            , effects = bodyResult.effects ++ acc.effects
                                            }

                            else
                                Ok acc
            in
            go
                { effects = []
                , envChain = envChain
                }

        Print expr ->
            -- TODO: likely we'll need to handle effects here
            interpretExpr envChain expr
                |> Result.mapError (\err -> ( err, [] ))
                |> Result.map
                    (\res ->
                        { envChain = res.envChain
                        , effects = [ PrintEff (Value.toString res.value) ]
                        }
                    )

        VarDecl name maybeExpr ->
            case maybeExpr of
                Nothing ->
                    Ok
                        { envChain = Env.declareInCurrent name VNil envChain
                        , effects = []
                        }

                Just expr ->
                    -- TODO: likely we'll need to handle effects here
                    interpretExpr envChain expr
                        |> Result.mapError (\err -> ( err, [] ))
                        |> Result.map
                            (\res ->
                                { envChain = Env.declareInCurrent name res.value res.envChain
                                , effects = []
                                }
                            )

        Block statements ->
            List.foldl
                (\statement result ->
                    case result of
                        Err ( err, effects ) ->
                            Err ( err, effects )

                        Ok acc ->
                            case interpretStmt acc.envChain statement of
                                Err ( err, effects ) ->
                                    Err ( err, effects ++ acc.effects )

                                Ok res ->
                                    Ok
                                        { envChain = res.envChain
                                        , effects = res.effects ++ acc.effects
                                        }
                )
                (Ok
                    { envChain = Env.pushEmptyEnv envChain
                    , effects = []
                    }
                )
                statements
                |> Result.map
                    (\res ->
                        { envChain = Env.popEnv res.envChain
                        , effects = res.effects
                        }
                    )


interpretExpr : EnvChain -> Expr -> Result Error { envChain : EnvChain, value : Value }
interpretExpr envChain expr =
    {- TODO we'll likely later add effects / ... to the `value` returned record
       if not, we can simplify a lot of things here!

       When adding effects to the returned record please rename the `value` field
       so that you can check if you need to _not throw away_ some effects from
       children `interpretExpr` calls.
    -}
    let
        withoutEnvChanges : Value -> Result Error { envChain : EnvChain, value : Value }
        withoutEnvChanges value =
            Ok { envChain = envChain, value = value }
    in
    case expr of
        Nil ->
            withoutEnvChanges VNil

        False_ ->
            withoutEnvChanges (VBool False)

        True_ ->
            withoutEnvChanges (VBool True)

        LiteralString str ->
            withoutEnvChanges (VString str)

        LiteralNumber float ->
            withoutEnvChanges (VFloat float)

        Grouping e ->
            interpretExpr envChain e

        Identifier name ->
            case Env.get name envChain of
                Nothing ->
                    Err <|
                        Error.error
                            -1
                            (InterpreterError <| UnknownIdentifier name)

                Just value ->
                    withoutEnvChanges value

        LogicOr left right ->
            interpretExpr envChain left
                |> Result.andThen
                    (\left_ ->
                        if Value.isTruthy left_.value then
                            Ok left_

                        else
                            interpretExpr left_.envChain right
                    )

        LogicAnd left right ->
            interpretExpr envChain left
                |> Result.andThen
                    (\left_ ->
                        if Value.isTruthy left_.value then
                            interpretExpr left_.envChain right

                        else
                            Ok left_
                    )

        Unary { operator, right } ->
            let
                tokenType : Token.Type
                tokenType =
                    Token.type_ operator
            in
            case tokenType of
                Token.Minus ->
                    interpretExpr envChain right
                        |> Result.andThen
                            (\r ->
                                floatNegate operator r.value
                                    |> Result.map (\newValue -> { r | value = newValue })
                            )

                Token.Bang ->
                    interpretExpr envChain right
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

                numOp : (Float -> Float -> Float) -> Result Error { envChain : EnvChain, value : Value }
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
                        (interpretExpr envChain left)
                        (interpretExpr envChain right)
                        |> Result.andThen identity

                comparisonOp : (Float -> Float -> Bool) -> Result Error { envChain : EnvChain, value : Value }
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
                        (interpretExpr envChain left)
                        (interpretExpr envChain right)
                        |> Result.andThen identity

                equalityOp : (Value -> Value -> Bool) -> Result Error { envChain : EnvChain, value : Value }
                equalityOp op =
                    Result.map2 (\l r -> setValue (VBool (op l.value r.value)) r)
                        (interpretExpr envChain left)
                        (interpretExpr envChain right)
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
                        (interpretExpr envChain left)
                        (interpretExpr envChain right)
                        |> Result.andThen identity

                _ ->
                    Err
                        (Error.error
                            (Token.line operator)
                            (InterpreterError <| UnexpectedBinaryOperator tokenType)
                        )

        Assign { names, value } ->
            interpretExpr envChain value
                |> Result.andThen
                    (\exprResult ->
                        List.foldl
                            (\name accResult ->
                                accResult
                                    |> Result.andThen
                                        (\acc ->
                                            acc.envChain
                                                |> Env.assignToExisting name acc.value
                                                |> Maybe.map (\newChain -> { acc | envChain = newChain })
                                                |> Result.fromMaybe (Error.error -1 (InterpreterError (UnknownIdentifier name)))
                                        )
                            )
                            (Ok exprResult)
                            names
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
