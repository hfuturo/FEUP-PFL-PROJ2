module Tests where
import Main (testAssembler, testParser)
import Lexer (Token)
import Action
    ( Inst(Equ, Le, Loop, Sub, Mult, Branch, Store, Fetch, Fals, Push,
           Tru, Neg) )

import Data.List (elemIndices)

type TestResults = Either Bool [Int]

runAssemblerTests :: TestResults
runAssemblerTests
    = if and results then Left True else Right (map (+1) (elemIndices False results))
    where results = [testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")] ++
                    [testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")] ++
                    [testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")] ++
                    [testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")] ++
                    [testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")] ++
                    [testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")] ++
                    [testAssembler [Push (-20),Push (-21), Le] == ("True","")] ++
                    [testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")] ++
                    [testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")] ++
                    [testAssembler [Tru, Branch [Push 10, Push 4, Push 3, Sub, Mult] [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]] == ("-10","")] ++
                    [testAssembler [Fals, Branch [Push 10, Push 4, Push 3, Sub, Mult] [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]] == ("","a=3,someVar=False,var=True")] ++
                    [testAssembler [Tru, Tru, Branch [Branch [Fals,Store "var",Fetch "var"] [Push (-20),Tru,Fals]] [Push (-20),Tru,Tru,Neg,Equ]] == ("False","var=False")] ++
                    [testAssembler [Tru, Branch [Fals, Branch [Fals,Store "var",Fetch "var"] [Push (-20),Tru,Fals]] [Push (-20),Tru,Tru,Neg,Equ]] == ("False,True,-20","")]

-- Tests que dao return a "Run-time error"
-- yes: testAssembler [Push 1,Push 2,And] == "Run-time error"
-- yes: testAssembler [Tru,Tru,Store "y", Fetch "x",Tru] == "Run-time error"

runParserTests :: TestResults
runParserTests
    = if and results then Left True else Right (map (+1) (elemIndices False results))
    where results = [testParser "x := 5; x := x - 1;" == ("","x=4")] ++
                    [testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")] ++
                    [testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")] ++
                    [testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")] ++
                    [testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")] ++
                    [testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")] ++
                    [testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")] ++
                    [testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")] ++
                    [testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")] ++
                    [testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")] ++
                    [testParser "x := True; if x = True then (x := 33; x := x+1;) else x := 1;" == ("","x=34")] ++
                    [testParser "x := True; if True = x then (x := 33; x := x+1;) else x := 1;" == ("","x=34")] ++
                    [testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")]