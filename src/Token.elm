module Token exposing
    ( Token
    , Type(..)
    , getIdentifier
    , getNumber
    , getString
    , isIdentifier
    , isNumber
    , isString
    , lexeme
    , line
    , toString
    , token
    , typeToString
    , type_
    )


type Token
    = Token
        { type_ : Type
        , lexeme : String -- TODO are lexemes needed?
        , line : Int
        }


token : Type -> String -> Int -> Token
token type__ lexeme_ line_ =
    Token
        { type_ = type__
        , lexeme = lexeme_
        , line = line_
        }


type Type
    = LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Identifier String
    | String String
    | Number Float
    | And
    | Class
    | Else
    | False_
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True_
    | Var
    | While


typeToString : Type -> String
typeToString type__ =
    case type__ of
        LeftParen ->
            "LeftParen"

        RightParen ->
            "RightParen"

        LeftBrace ->
            "LeftBrace"

        RightBrace ->
            "RightBrace"

        Comma ->
            "Comma"

        Dot ->
            "Dot"

        Minus ->
            "Minus"

        Plus ->
            "Plus"

        Semicolon ->
            "Semicolon"

        Slash ->
            "Slash"

        Star ->
            "Star"

        Bang ->
            "Bang"

        BangEqual ->
            "BangEqual"

        Equal ->
            "Equal"

        EqualEqual ->
            "EqualEqual"

        Greater ->
            "Greater"

        GreaterEqual ->
            "GreaterEqual"

        Less ->
            "Less"

        LessEqual ->
            "LessEqual"

        Identifier identifier ->
            "Identifier: " ++ identifier

        String string ->
            "String: " ++ string

        Number float ->
            "Number: " ++ String.fromFloat float

        And ->
            "And"

        Class ->
            "Class"

        Else ->
            "Else"

        False_ ->
            "False"

        Fun ->
            "Fun"

        For ->
            "For"

        If ->
            "If"

        Nil ->
            "Nil"

        Or ->
            "Or"

        Print ->
            "Print"

        Return ->
            "Return"

        Super ->
            "Super"

        This ->
            "This"

        True_ ->
            "True"

        Var ->
            "Var"

        While ->
            "While"


toString : Token -> String
toString (Token t) =
    typeToString t.type_


type_ : Token -> Type
type_ (Token t) =
    t.type_


lexeme : Token -> String
lexeme (Token t) =
    t.lexeme


isNumber : Token -> Bool
isNumber (Token t) =
    case t.type_ of
        Number _ ->
            True

        _ ->
            False


isString : Token -> Bool
isString (Token t) =
    case t.type_ of
        String _ ->
            True

        _ ->
            False


isIdentifier : Token -> Bool
isIdentifier (Token t) =
    case t.type_ of
        Identifier _ ->
            True

        _ ->
            False


getNumber : Token -> Maybe Float
getNumber (Token t) =
    case t.type_ of
        Number n ->
            Just n

        _ ->
            Nothing


getString : Token -> Maybe String
getString (Token t) =
    case t.type_ of
        String str ->
            Just str

        _ ->
            Nothing


getIdentifier : Token -> Maybe String
getIdentifier (Token t) =
    case t.type_ of
        Identifier str ->
            Just str

        _ ->
            Nothing


line : Token -> Int
line (Token t) =
    t.line
