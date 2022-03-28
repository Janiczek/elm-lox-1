module Tests exposing
    ( astPrinterTests
    , interpreterExprTests
    , interpreterProgramTests
    , parserTests
    , scannerTests
    )

import AstPrinter
import Effect exposing (Effect(..))
import Env
import Error exposing (Error)
import Expect
import Expr
import Interpreter
import Parser
import Scanner
import Stmt exposing (Stmt)
import Test exposing (Test)
import Token exposing (Type(..))
import Value exposing (Value(..))


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
                        |> Scanner.scan
                        |> Result.map (List.map Token.type_)
                        |> Expect.equal (Ok output)
    in
    Test.describe "Scanner.scan"
        -- TODO some tests that show how newline and token.line handling works
        [ okCases
            |> List.map runOkCase
            |> Test.describe "OK cases"
        ]


parserTests : Test
parserTests =
    let
        parseSource : String -> Result Error (List Stmt)
        parseSource source =
            source
                |> Scanner.scan
                |> Result.andThen Parser.parseProgram
    in
    Test.describe "Parser.parseProgram"
        [ Test.test "for (init;cond;incr)" <|
            \() ->
                """
                for (var i = 0; i < 5; i = i + 1)
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.VarDecl "i" (Just (Expr.LiteralNumber 0))
                                , Stmt.While
                                    { condition =
                                        Expr.Binary
                                            { left = Expr.Identifier "i"
                                            , operator = Token.token Token.Less "<" 2
                                            , right = Expr.LiteralNumber 5
                                            }
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            , Stmt.ExprStmt
                                                (Expr.Assign
                                                    { names = [ "i" ]
                                                    , value =
                                                        Expr.Binary
                                                            { left = Expr.Identifier "i"
                                                            , operator = Token.token Token.Plus "+" 2
                                                            , right = Expr.LiteralNumber 1
                                                            }
                                                    }
                                                )
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (init;cond;)" <|
            \() ->
                """
                for (var i = 0; i < 5;) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.VarDecl "i" (Just (Expr.LiteralNumber 0))
                                , Stmt.While
                                    { condition =
                                        Expr.Binary
                                            { left = Expr.Identifier "i"
                                            , operator = Token.token Token.Less "<" 2
                                            , right = Expr.LiteralNumber 5
                                            }
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (init;;incr)" <|
            \() ->
                """
                for (var i = 0;; i = i + 1) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.VarDecl "i" (Just (Expr.LiteralNumber 0))
                                , Stmt.While
                                    { condition = Expr.True_
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            , Stmt.ExprStmt
                                                (Expr.Assign
                                                    { names = [ "i" ]
                                                    , value =
                                                        Expr.Binary
                                                            { left = Expr.Identifier "i"
                                                            , operator = Token.token Token.Plus "+" 2
                                                            , right = Expr.LiteralNumber 1
                                                            }
                                                    }
                                                )
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (init;;)" <|
            \() ->
                """
                for (var i = 0;;) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.VarDecl "i" (Just (Expr.LiteralNumber 0))
                                , Stmt.While
                                    { condition = Expr.True_
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (;cond;incr)" <|
            \() ->
                """
                for (; i < 5; i = i + 1) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.While
                                    { condition =
                                        Expr.Binary
                                            { left = Expr.Identifier "i"
                                            , operator = Token.token Token.Less "<" 2
                                            , right = Expr.LiteralNumber 5
                                            }
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            , Stmt.ExprStmt
                                                (Expr.Assign
                                                    { names = [ "i" ]
                                                    , value =
                                                        Expr.Binary
                                                            { left = Expr.Identifier "i"
                                                            , operator = Token.token Token.Plus "+" 2
                                                            , right = Expr.LiteralNumber 1
                                                            }
                                                    }
                                                )
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (;cond;)" <|
            \() ->
                """
                for (; i < 5;) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.While
                                    { condition =
                                        Expr.Binary
                                            { left = Expr.Identifier "i"
                                            , operator = Token.token Token.Less "<" 2
                                            , right = Expr.LiteralNumber 5
                                            }
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (;;incr)" <|
            \() ->
                """
                for (;; i = i + 1) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.While
                                    { condition = Expr.True_
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            , Stmt.ExprStmt
                                                (Expr.Assign
                                                    { names = [ "i" ]
                                                    , value =
                                                        Expr.Binary
                                                            { left = Expr.Identifier "i"
                                                            , operator = Token.token Token.Plus "+" 2
                                                            , right = Expr.LiteralNumber 1
                                                            }
                                                    }
                                                )
                                            ]
                                    }
                                ]
                            ]
                        )
        , Test.test "for (;;)" <|
            \() ->
                """
                for (;;) 
                    print i;
                """
                    |> parseSource
                    |> Expect.equal
                        (Ok
                            [ Stmt.Block
                                [ Stmt.While
                                    { condition = Expr.True_
                                    , body =
                                        Stmt.Block
                                            [ Stmt.Print (Expr.Identifier "i")
                                            ]
                                    }
                                ]
                            ]
                        )
        ]


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
                    |> AstPrinter.printExpr
                    |> Expect.equal "(* (- 123) (group 45.67))"
        ]


interpreterExprTests : Test
interpreterExprTests =
    let
        interpretSource : String -> Result Error Value
        interpretSource source =
            source
                |> Scanner.scan
                |> Result.andThen Parser.parseExpr
                |> Result.andThen (Interpreter.interpretExpr Env.empty)
                |> Result.map .value
    in
    Test.describe "Interpreter.interpretExpr"
        [ Test.test "logical or - normal" <|
            \() ->
                "false or 42"
                    |> interpretSource
                    |> Expect.equal (Ok (VFloat 42))
        , Test.test "logical or - short circuit" <|
            \() ->
                "true or 42"
                    |> interpretSource
                    |> Expect.equal (Ok (VBool True))
        , Test.test "logical and - normal" <|
            \() ->
                "true and 42"
                    |> interpretSource
                    |> Expect.equal (Ok (VFloat 42))
        , Test.test "logical and - short circuit" <|
            \() ->
                "false and print 1"
                    |> interpretSource
                    |> Expect.equal (Ok (VBool False))

        -- TODO and or combined
        ]


interpreterProgramTests : Test
interpreterProgramTests =
    let
        interpretSource : String -> Result ( Error, List Effect ) (List Effect)
        interpretSource source =
            source
                |> Scanner.scan
                |> Result.andThen Parser.parseProgram
                |> Result.mapError (\err -> ( err, [] ))
                |> Result.andThen Interpreter.interpretProgram
    in
    Test.describe "Interpreter.interpretProgram"
        [ Test.test "two blocks" <|
            \() ->
                """
                {
                  var a = "first";
                  print a; // "first"
                }
                {
                  var a = "second";
                  print a; // "second"
                }
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "\"first\""
                            , PrintEff "\"second\""
                            ]
                        )
        , Test.test "assignment right before print" <|
            \() ->
                """
                var a = 1;
                print a = 2; // "2"
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "2" ]
                        )
        , Test.test "override of outer from inner" <|
            \() ->
                """
                var x = 1;
                {
                  print x; // 1
                  x = 2;
                  print x; // 2

                  var y = 1;
                  print y; // 1
                }
                print x; // 2
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "1"
                            , PrintEff "2"
                            , PrintEff "1"
                            , PrintEff "2"
                            ]
                        )
        , Test.test "nested block" <|
            \() ->
                """
                var x = 1;
                print x; // 1
                {
                  print x; // 1
                  x = 2;
                  print x; // 2
                  {
                      print x; // 2
                      x = 3;
                      print x; // 3
                  }
                  print x; // 3
                }
                print x; // 3
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "1"
                            , PrintEff "1"
                            , PrintEff "2"
                            , PrintEff "2"
                            , PrintEff "3"
                            , PrintEff "3"
                            , PrintEff "3"
                            ]
                        )
        , Test.test "inner decl doesn't leak to outer" <|
            \() ->
                """
                {
                  var a = "in block";
                  print a; // "in block"
                }
                print a; // Error: No "a"
                """
                    |> interpretSource
                    |> Expect.equal
                        (Err
                            -- TODO correct line number (5)
                            ( Error.error -1 (Error.InterpreterError (Error.UnknownIdentifier "a"))
                            , [ PrintEff "\"in block\"" ]
                            )
                        )
        , Test.test "effect order in errors" <|
            \() ->
                """
                print 1;
                print 2;
                {
                  var a = "in block";
                  print 3;
                  print 4;
                  {
                      print 5;
                      print 6;
                  }
                }
                {
                    print 7;
                    print 8;
                }
                print 9;
                print 10;
                print a; // Error: No "a"
                """
                    |> interpretSource
                    |> Expect.equal
                        (Err
                            -- TODO correct line number (5)
                            ( Error.error -1 (Error.InterpreterError (Error.UnknownIdentifier "a"))
                            , [ PrintEff "1"
                              , PrintEff "2"
                              , PrintEff "3"
                              , PrintEff "4"
                              , PrintEff "5"
                              , PrintEff "6"
                              , PrintEff "7"
                              , PrintEff "8"
                              , PrintEff "9"
                              , PrintEff "10"
                              ]
                            )
                        )
        , Test.test "encapsulation of var inside block" <|
            \() ->
                """
                var volume = 11;
                volume = 0;

                {
                  var volume = 3 * 4 * 5;
                  print volume; // 60
                }

                print volume; // 0
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "60"
                            , PrintEff "0"
                            ]
                        )
        , Test.test "big example from the book" <|
            \() ->
                """
                var a = "global a";
                var b = "global b";
                var c = "global c";
                {
                  var a = "outer a";
                  var b = "outer b";
                  {
                    var a = "inner a";
                    print a;
                    print b;
                    print c;
                  }
                  print a;
                  print b;
                  print c;
                }
                print a;
                print b;
                print c;
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "\"inner a\""
                            , PrintEff "\"outer b\""
                            , PrintEff "\"global c\""
                            , PrintEff "\"outer a\""
                            , PrintEff "\"outer b\""
                            , PrintEff "\"global c\""
                            , PrintEff "\"global a\""
                            , PrintEff "\"global b\""
                            , PrintEff "\"global c\""
                            ]
                        )
        , Test.test "if (then)" <|
            \() ->
                """
                if (1) print 10; else print 20;
                """
                    |> interpretSource
                    |> Expect.equal (Ok [ PrintEff "10" ])
        , Test.test "if (else)" <|
            \() ->
                """
                if (false) print 10; else print 20;
                """
                    |> interpretSource
                    |> Expect.equal (Ok [ PrintEff "20" ])
        , Test.test "if without else (then)" <|
            \() ->
                """
                if (true) print 10;
                """
                    |> interpretSource
                    |> Expect.equal (Ok [ PrintEff "10" ])
        , Test.test "if without else (else)" <|
            \() ->
                """
                if (false) print 10;
                """
                    |> interpretSource
                    |> Expect.equal (Ok [])
        , Test.test "logic or (example from book)" <|
            \() ->
                """
                print "hi" or 2; // "hi"
                print nil or "yes"; // "yes"
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "\"hi\""
                            , PrintEff "\"yes\""
                            ]
                        )
        , Test.test "while" <|
            \() ->
                """
                var i = 5;
                while (i > 0) {
                  print i;
                  i = i - 1;
                }
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "5"
                            , PrintEff "4"
                            , PrintEff "3"
                            , PrintEff "2"
                            , PrintEff "1"
                            ]
                        )
        , Test.test "for" <|
            \() ->
                """
                for (var i = 0; i < 5; i = i + 1) {
                    print i;
                }
                """
                    |> interpretSource
                    |> Expect.equal
                        (Ok
                            [ PrintEff "0"
                            , PrintEff "1"
                            , PrintEff "2"
                            , PrintEff "3"
                            , PrintEff "4"
                            ]
                        )
        ]
