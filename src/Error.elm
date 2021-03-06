module Error exposing
    ( Bug(..)
    , Error
    , InterpreterError(..)
    , ParserError(..)
    , ScannerError(..)
    , Type(..)
    , error
    , isInterpreterError
    , locatedError
    , toString
    )

import Token
import Value exposing (Value)


type Error
    = Error
        { line : Int
        , type_ : Type
        }


type Type
    = ScannerError ScannerError
    | ParserError ParserError
    | InterpreterError InterpreterError
    | Bug Bug


type ScannerError
    = UnexpectedCharacter String
    | UnterminatedString


type ParserError
    = EmptyOneOf
    | ExpectedToken Token.Type
    | ExpectedNumberP
    | ExpectedStringP
    | ExpectedIdentifier
    | ExpectedEOF
    | ExpectedNonemptyList


type InterpreterError
    = UnexpectedUnaryOperator Token.Type
    | UnexpectedBinaryOperator Token.Type
    | ExpectedNumberI Value
    | ExpectedStringI Value
    | ExpectedNumberOrString Value
    | UnknownIdentifier String


type Bug
    = ScannedFloatCouldntBeConvertedFromString


error : Int -> Type -> Error
error line type_ =
    Error
        { line = line
        , type_ = type_
        }


locatedError :
    { line : Int
    , type_ : Type
    }
    -> Error
locatedError rec =
    Error rec


typeToString : Type -> String
typeToString type_ =
    case type_ of
        ScannerError scannerError ->
            let
                string =
                    case scannerError of
                        UnexpectedCharacter char ->
                            "Unexpected character: "
                                ++ String.replace "\n" "\\n" char
                                ++ "."

                        UnterminatedString ->
                            "Unterminated string"
            in
            "[SCANNER] " ++ string

        ParserError parserError ->
            let
                string =
                    case parserError of
                        EmptyOneOf ->
                            "Empty oneOf"

                        ExpectedToken tokenType ->
                            "Expected token: " ++ Token.typeToString tokenType

                        ExpectedNumberP ->
                            "Expected number"

                        ExpectedStringP ->
                            "Expected string"

                        ExpectedIdentifier ->
                            "Expected identifier"

                        ExpectedEOF ->
                            "Expected EOF"

                        ExpectedNonemptyList ->
                            "Expected non-empty list"
            in
            "[PARSER] " ++ string

        InterpreterError interpreterError ->
            let
                string =
                    case interpreterError of
                        UnexpectedUnaryOperator tokenType ->
                            "Unexpected unary operator: "
                                ++ Token.typeToString tokenType

                        UnexpectedBinaryOperator tokenType ->
                            "Unexpected binary operator: "
                                ++ Token.typeToString tokenType

                        ExpectedNumberI value ->
                            "Expected number but got "
                                ++ Value.type_ value
                                ++ ": "
                                ++ Value.toString value

                        ExpectedStringI value ->
                            "Expected string but got "
                                ++ Value.type_ value
                                ++ ": "
                                ++ Value.toString value

                        ExpectedNumberOrString value ->
                            "Expected number or string but got "
                                ++ Value.type_ value
                                ++ ": "
                                ++ Value.toString value

                        UnknownIdentifier name ->
                            "Unknown identifier: " ++ name
            in
            "[INTERPRETER] " ++ string

        Bug bug ->
            let
                bugString =
                    case bug of
                        ScannedFloatCouldntBeConvertedFromString ->
                            "Scanned float couldn't be converted from string"
            in
            "[BUG] " ++ bugString


toString : Error -> String
toString (Error { line, type_ }) =
    "[line "
        ++ String.fromInt line
        ++ "] Error: "
        ++ typeToString type_


isInterpreterError : Error -> Bool
isInterpreterError (Error { type_ }) =
    case type_ of
        InterpreterError _ ->
            True

        ScannerError _ ->
            False

        ParserError _ ->
            False

        Bug _ ->
            False
