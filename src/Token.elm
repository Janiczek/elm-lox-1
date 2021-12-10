module Token exposing (Token, Type(..), lexeme, toString, token, type_)


type Token
    = Token
        { type_ : Type
        , lexeme : String -- TODO are lexemes needed?
        , line : Int
        }


token : Type -> String -> Int -> Token
token type__ lexeme_ line =
    Token
        { type_ = type__
        , lexeme = lexeme_
        , line = line
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
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | EOF


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

        False ->
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

        True ->
            "True"

        Var ->
            "Var"

        While ->
            "While"

        EOF ->
            "EOF"


toString : Token -> String
toString (Token t) =
    typeToString t.type_


type_ : Token -> Type
type_ (Token t) =
    t.type_


lexeme : Token -> String
lexeme (Token t) =
    t.lexeme
