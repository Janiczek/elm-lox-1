module Tests exposing (scannerTests)

import Error exposing (Error)
import Expect
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

            -- TODO identifier
            -- TODO and
            -- TODO class
            -- TODO else
            -- TODO false
            -- TODO fun
            -- TODO for
            -- TODO if
            -- TODO nil
            -- TODO or
            -- TODO print
            -- TODO return
            -- TODO super
            -- TODO this
            -- TODO true
            -- TODO var
            -- TODO while
            -- TODO EOF
            ]

        errCases : List ( String, String, List Error.Type )
        errCases =
            -- TODO
            -- TODO number with decimal point but nothing after
            -- TODO number with decimal point but nothing before
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
        [ okCases
            |> List.map runOkCase
            |> Test.describe "OK cases"
        ]
