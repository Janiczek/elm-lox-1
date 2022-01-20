module Interpreter exposing (interpret)

import Error exposing (Error, InterpreterError(..), Type(..))
import Expr exposing (Expr(..))
import Token exposing (Token)
import Value exposing (Value(..))


interpret : Expr -> Result (List Error) Value
interpret expr =
    case expr of
        Nil ->
            Ok VNil

        False_ ->
            Ok <| VBool False

        True_ ->
            Ok <| VBool True

        LiteralString str ->
            Ok <| VString str

        LiteralNumber float ->
            Ok <| VFloat float

        Grouping e ->
            interpret e

        Identifier str ->
            Debug.todo "interpret identifier"

        Unary { operator, right } ->
            let
                tokenType : Token.Type
                tokenType =
                    Token.type_ operator
            in
            case tokenType of
                Token.Minus ->
                    interpret right
                        |> Result.andThen (floatNegate operator)

                Token.Bang ->
                    interpret right
                        |> Result.map boolNegate

                _ ->
                    Err
                        [ Error.error
                            (Token.line operator)
                            (InterpreterError <| UnexpectedUnaryOperator tokenType)
                        ]

        Binary { left, operator, right } ->
            let
                tokenType : Token.Type
                tokenType =
                    Token.type_ operator

                numOp : (Float -> Float -> Float) -> Result (List Error) Value
                numOp op =
                    Result.map2
                        (\leftExpr rightExpr ->
                            case ( leftExpr, rightExpr ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| VFloat (op leftFloat rightFloat)

                                ( _, VFloat _ ) ->
                                    -- left is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI leftExpr)
                                        ]

                                _ ->
                                    -- right is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI rightExpr)
                                        ]
                        )
                        (interpret left)
                        (interpret right)
                        |> Result.andThen identity

                comparisonOp : (Float -> Float -> Bool) -> Result (List Error) Value
                comparisonOp op =
                    Result.map2
                        (\leftExpr rightExpr ->
                            case ( leftExpr, rightExpr ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| VBool (op leftFloat rightFloat)

                                ( _, VFloat _ ) ->
                                    -- left is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI leftExpr)
                                        ]

                                _ ->
                                    -- right is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI rightExpr)
                                        ]
                        )
                        (interpret left)
                        (interpret right)
                        |> Result.andThen identity

                equalityOp : (Value -> Value -> Bool) -> Result (List Error) Value
                equalityOp op =
                    Result.map2 op
                        (interpret left)
                        (interpret right)
                        |> Result.map VBool
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
                        (\leftExpr rightExpr ->
                            case ( leftExpr, rightExpr ) of
                                ( VFloat leftFloat, VFloat rightFloat ) ->
                                    Ok <| VFloat (leftFloat + rightFloat)

                                ( VString leftString, VString rightString ) ->
                                    Ok <| VString (leftString ++ rightString)

                                ( VFloat _, _ ) ->
                                    -- right is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberI rightExpr)
                                        ]

                                ( VString _, _ ) ->
                                    -- right is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedStringI rightExpr)
                                        ]

                                _ ->
                                    -- left is faulty
                                    Err
                                        [ Error.error
                                            (Token.line operator)
                                            (InterpreterError <| ExpectedNumberOrString leftExpr)
                                        ]
                        )
                        (interpret left)
                        (interpret right)
                        |> Result.andThen identity

                _ ->
                    Err
                        [ Error.error
                            (Token.line operator)
                            (InterpreterError <| UnexpectedBinaryOperator tokenType)
                        ]


floatNegate : Token -> Value -> Result (List Error) Value
floatNegate operator value =
    case value of
        VFloat num ->
            Ok (VFloat (negate num))

        _ ->
            Err
                [ Error.error
                    (Token.line operator)
                    (InterpreterError <| ExpectedNumberI value)
                ]


boolNegate : Value -> Value
boolNegate value =
    VBool (not (Value.isTruthy value))
