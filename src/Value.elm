module Value exposing (Value(..), isTruthy, toString, type_)


type Value
    = VFloat Float
    | VString String
    | VBool Bool
    | VNil


toString : Value -> String
toString value =
    case value of
        VFloat float ->
            String.fromFloat float

        VString string ->
            -- TODO escape the quotes inside the string?
            "\"" ++ string ++ "\""

        VBool bool ->
            if bool then
                "true"

            else
                "false"

        VNil ->
            "nil"


type_ : Value -> String
type_ value =
    case value of
        VFloat _ ->
            "number"

        VString _ ->
            "string"

        VBool _ ->
            "bool"

        VNil ->
            "nil"


isTruthy : Value -> Bool
isTruthy value =
    case value of
        VFloat _ ->
            True

        VString _ ->
            True

        VBool bool ->
            bool

        VNil ->
            False
