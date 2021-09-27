module Error exposing (Error, error, locatedError, toString)


type Error
    = Error { line : Int, where_ : Maybe String, message : String }


error : Int -> String -> Error
error line message =
    Error
        { line = line
        , where_ = Nothing
        , message = message
        }


locatedError : { line : Int, where_ : Maybe String, message : String } -> Error
locatedError rec =
    Error rec


toString : Error -> String
toString (Error { line, where_, message }) =
    "[line "
        ++ String.fromInt line
        ++ "] Error"
        ++ Maybe.withDefault "" where_
        ++ ": "
        ++ message
