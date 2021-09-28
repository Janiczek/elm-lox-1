module Error exposing
    ( Bug(..)
    , Error
    , Type(..)
    , error
    , locatedError
    , toString
    )


type Error
    = Error
        { line : Int
        , where_ : Maybe String
        , type_ : Type
        }


type Type
    = UnexpectedCharacter String
    | UnterminatedString
    | Bug Bug


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
        UnexpectedCharacter char ->
            "Unexpected character: "
                ++ String.replace "\n" "\\n" char
                ++ "."

        UnterminatedString ->
            "Unterminated string"

        Bug bug ->
            let
                bugString =
                    case bug of
                        ScannedFloatCouldntBeConvertedFromString ->
                            "scanned float couldn't be converted from string"
            in
            "Bug: " ++ bugString


toString : Error -> String
toString (Error { line, where_, type_ }) =
    "[line "
        ++ String.fromInt line
        ++ "] Error"
        ++ Maybe.withDefault "" where_
        ++ ": "
        ++ typeToString type_
