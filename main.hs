import Data.List ( sort, intercalate )
import Inst
import Action
import Debug.Trace

-- deal with stack
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map valueToStr (reverse stack)

valueToStr :: Value -> String
valueToStr (Left x) = show x
valueToStr (Right s) 
  | s == "ff" = show False
  | s == "tt" = show True
  | otherwise = s

-- deal with state
createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," $ map (\(x, y) -> x ++ "=" ++ (valueToStr y)) (sort state)

-- run :: (Code, Stack, State) -> IO (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((xi:xf), stack, state)
  | isLoop xi =
    run (xf ++ (loopC1Var xi) ++ [Branch ((loopC2Var xi) ++ [xi]) [Noop]], stack, state)
  | isBranch xi && (last stack) == Right "tt" = 
    run (xf ++ (branchC1Var xi), init stack, state)
  | isBranch xi && (last stack) == Right "ff" = 
    run (xf ++ (branchC2Var xi), init stack, state)
  | isPush xi =
    run (xf, stack ++ [Left (pushValue xi)], state)
  | show xi == "Noop" = 
    run (xf, stack, state)
  | show xi == "Fals" =
    run (xf, stack ++ [Right "ff"], state)
  | show xi == "Tru" =
    run (xf, stack ++ [Right "tt"], state)
  | isStore xi =
    run (xf, init stack, storeOperation state (storeVar xi) (last stack))
  | isFetch xi =
    run (xf, stack ++ [fetchOperation state (fetchVar xi)], state)
  | show xi == "Add" =
    run (xf, take (length stack - 2) stack ++ [addOperation (last stack) (last (init stack))], state)
  | show xi == "Sub" =
    run (xf, take (length stack - 2) stack ++ [subOperation (last stack) (last (init stack))], state)
  | show xi == "Mult" =
    run (xf, take (length stack - 2) stack ++ [multOperation (last stack) (last (init stack))], state)
  | show xi == "Neg" =
    run (xf, init stack ++ [negOperation (last stack)], state)
  | show xi == "Equ" =
    run (xf, take (length stack - 2) stack ++ [(equOperation (last stack) (last (init stack)))],state)
  | show xi == "Le" =
    run (xf, take (length stack - 2) stack ++ [(leOperation (last stack) (last (init stack)))],state)
  | otherwise = error "Run-time error"

-- To help you test your assembler
-- testAssembler :: Code -> IO (String, String)
-- testAssembler code = do
--   (_, stack, state) <- run (code, createEmptyStack, createEmptyState)
--   return (stack2Str stack, state2Str state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- yes : testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- yes : testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- yes : testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- yes : testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- yes : testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- yes : testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- yes : testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- yes : testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- yes : testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- yes : testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- yes : testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"
-- yes : testAssembler [Tru, Branch [Push 10, Push 4, Push 3, Sub, Mult] [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]] == ("-10","")
-- tes : testAssembler [Fals, Branch [Push 10, Push 4, Push 3, Sub, Mult] [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]] == ("","a=3,someVar=False,var=True")
-- tes : testAssembler [Tru, Tru, Branch [Branch [Fals,Store "var",Fetch "var"] [Push (-20),Tru,Fals]] [Push (-20),Tru,Tru,Neg,Equ]] == ("False","var=False")
-- tes : testAssembler [Tru, Branch [Fals, Branch [Fals,Store "var",Fetch "var"] [Push (-20),Tru,Fals]] [Push (-20),Tru,Tru,Neg,Equ]] == ("False,True,-20","")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- lexer :: String -> [Token]
-- lexer [] = []
-- lexer ('+' : restStr) = PlusTok : lexer restStr
-- lexer ('*' : restStr) = TimesTok : lexer restStr
-- lexer ('(' : restStr) = OpenTok : lexer restStr
-- lexer (')' : restStr) = CloseTok : lexer restStr
-- lexer (chr : restStr)
--   | isSpace chr = lexer restStr
-- lexer (_ : restString) = error ("unexpected character: '" ++ show chr ++ "'")

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, store2Str store)
  --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")