module Error exposing
    ( Bug(..)
    , Error
    , ParserError(..)
    , ScannerError(..)
    , Type(..)
    , error
    , locatedError
    , toString
    )

import Token


type Error
    = Error
        { line : Int
        , where_ : Maybe String
        , type_ : Type
        }


type Type
    = ScannerError ScannerError
    | ParserError ParserError
    | Bug Bug


type ScannerError
    = UnexpectedCharacter String
    | UnterminatedString


type ParserError
    = EmptyOneOf
    | ExpectedToken Token.Type
    | ExpectedNumber
    | ExpectedString


type Bug
    = ScannedFloatCouldntBeConvertedFromString


error : Int -> Type -> Error
error line type_ =
    Error
        { line = line
        , where_ = Nothing
        , type_ = type_
        }


locatedError :
    { line : Int
    , where_ : Maybe String
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

                        ExpectedNumber ->
                            "Expected number"

                        ExpectedString ->
                            "Expected string"
            in
            "[PARSER] " ++ string

        Bug bug ->
            let
                bugString =
                    case bug of
                        ScannedFloatCouldntBeConvertedFromString ->
                            "Scanned float couldn't be converted from string"
            in
            "[BUG] " ++ bugString


toString : Error -> String
toString (Error { line, where_, type_ }) =
    "[line "
        ++ String.fromInt line
        ++ "] Error"
        ++ Maybe.withDefault "" where_
        ++ ": "
        ++ typeToString type_
