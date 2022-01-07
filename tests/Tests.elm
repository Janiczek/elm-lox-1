module Tests exposing (astPrinterTests, scannerTests)

import AstPrinter
import Error exposing (Error)
import Expect
import Expr
import Scanner
import Test exposing (Test)
import Token exposing (Type(..))


scannerTests : Test
scannerTests =
    let
        okCases : List ( String, String, List Token.Type )
        okCases =
            [ ( "Left paren", "(", [ LeftParen ] )
            , ( "Right paren", ")", [ RightParen ] )
            , ( "Spaces are ignored", "( )", [ LeftParen, RightParen ] )
            , ( "Doesn't care about semantics", ")((", [ RightParen, LeftParen, LeftParen ] )
            , ( "Left brace", "{", [ LeftBrace ] )
            , ( "Right brace", "}", [ RightBrace ] )
            , ( "Comma", ",", [ Comma ] )
            , ( "Dot", ".", [ Dot ] )
            , ( "Minus", "-", [ Minus ] )
            , ( "Plus", "+", [ Plus ] )
            , ( "Semicolon", ";", [ Semicolon ] )
            , ( "Slash", "/", [ Slash ] )
            , ( "Line comment is ignored", "//", [] )
            , ( "Anything after line comment is ignored", "// 1 + 2", [] )
            , ( "Anything before line comment stays", "+ // 1 + 2", [ Plus ] )
            , ( "Star", "*", [ Star ] )
            , ( "Bang", "!", [ Bang ] )
            , ( "BangEqual", "!=", [ BangEqual ] )
            , ( "Bang followed by something not equal", "!!", [ Bang, Bang ] )
            , ( "Equal", "=", [ Equal ] )
            , ( "EqualEqual", "==", [ EqualEqual ] )
            , ( "Equal followed by something not equal", "=!", [ Equal, Bang ] )
            , ( "Greater", ">", [ Greater ] )
            , ( "GreaterEqual", ">=", [ GreaterEqual ] )
            , ( "Greater followed by something not equal", ">!", [ Greater, Bang ] )
            , ( "Less", "<", [ Less ] )
            , ( "LessEqual", "<=", [ LessEqual ] )
            , ( "Less followed by something not equal", "<!", [ Less, Bang ] )
            , ( "String", "\"abc\"", [ String "abc" ] )
            , ( "Empty string", "\"\"", [ String "" ] )
            , ( "Number", "123", [ Number 123 ] )
            , ( "Number with decimal point", "123.45", [ Number 123.45 ] )
            , ( "Identifier", "abcde", [ Identifier "abcde" ] )
            , ( "Identifier with uppercase first char", "Abcde", [ Identifier "Abcde" ] )
            , ( "Identifier with alphanum after first char", "a1B2C3", [ Identifier "a1B2C3" ] )
            , ( "Reserved word: and", "and", [ And ] )
            , ( "Reserved word: class", "class", [ Class ] )
            , ( "Reserved word: else", "else", [ Else ] )
            , ( "Reserved word: false", "false", [ False_ ] )
            , ( "Reserved word: fun", "fun", [ Fun ] )
            , ( "Reserved word: for", "for", [ For ] )
            , ( "Reserved word: if", "if", [ If ] )
            , ( "Reserved word: nil", "nil", [ Nil ] )
            , ( "Reserved word: or", "or", [ Or ] )
            , ( "Reserved word: print", "print", [ Print ] )
            , ( "Reserved word: return", "return", [ Return ] )
            , ( "Reserved word: super", "super", [ Super ] )
            , ( "Reserved word: this", "this", [ This ] )
            , ( "Reserved word: true", "true", [ True_ ] )
            , ( "Reserved word: var", "var", [ Var ] )
            , ( "Reserved word: while", "while", [ While ] )
            ]

        errCases : List ( String, String, List Error.Type )
        errCases =
            -- TODO number with decimal point but nothing after
            -- TODO number with decimal point but nothing before
            -- TODO identifier with first char == number
            []

        runOkCase : ( String, String, List Token.Type ) -> Test
        runOkCase ( label, input, output ) =
            Test.test label <|
                \() ->
                    input
                        |> Scanner.scanTokens
                        |> Result.map (List.map Token.type_)
                        |> Expect.equal (Ok (output ++ [ EOF ]))
    in
    Test.describe "Scanner.scanTokens"
        -- TODO some tests that show how newline and token.line handling works
        -- TODO EOF
        [ okCases
            |> List.map runOkCase
            |> Test.describe "OK cases"
        ]



-- TODO parser tests


astPrinterTests : Test
astPrinterTests =
    Test.describe "AstPrinter.print"
        -- TODO some more?
        -- TODO perhaps make the input the actual source code string?
        [ Test.test "Example from book" <|
            \() ->
                Expr.Binary
                    { left =
                        Expr.Unary
                            { operator = Token.token Minus "-" 1
                            , right = Expr.LiteralNumber 123
                            }
                    , operator = Token.token Star "*" 1
                    , right =
                        Expr.Unary
                            { operator = Token.token (Identifier "group") "group" 1
                            , right = Expr.LiteralNumber 45.67
                            }
                    }
                    |> AstPrinter.print
                    |> Expect.equal "(* (- 123) (group 45.67))"
        ]
