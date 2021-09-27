module Error exposing (Error, Type(..), error, locatedError, toString)


type Error
    = Error
        { line : Int
        , where_ : Maybe String
        , type_ : Type
        }


type Type
    = UnexpectedCharacter String


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


toString : Error -> String
toString (Error { line, where_, type_ }) =
    "[line "
        ++ String.fromInt line
        ++ "] Error"
        ++ Maybe.withDefault "" where_
        ++ ": "
        ++ typeToString type_
