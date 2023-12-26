import Data.List (sort)
import Data.List (intercalate)
import Inst
import Action

-- deal with stack
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map valueToStr (reverse stack)

valueToStr :: Value -> String
valueToStr (Left x) = show x
valueToStr (Right s) = s

-- deal with state
createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," $ map (\(x, y) -> x ++ "=" ++ (valueToStr y)) (sort state)

run :: (Code, Stack, State) -> IO (Code, Stack, State)
run ([], stack, state) = return ([], stack, state)
run ((xi:xf), stack, state)
  | isPush xi = 
    run (xf, stack ++ [Left (pushValue xi)], state)
  | show xi == "Fals" = 
    run (xf, stack ++ [Right "ff"], state)
  | show xi == "Tru" = 
    run (xf, stack ++ [Right "tt"], state)
  | isStore xi = 
    run (xf, init stack, state ++ [(storeVar xi, last stack)])
  | isFetch xi =
    run (xf, stack ++ [fetchOperation state (fetchVar xi)], state)
  | show xi == "Add" =
    run (xf, take (length stack - 2) stack ++ [addOperation (last stack) (last (init stack))], state)
  | show xi == "Sub" =
    run (xf, take (length stack - 2) stack ++ [subOperation(last stack) (last (init stack))], state)
  | show xi == "Mult" =
    run (xf, take (length stack - 2) stack ++ [multOperation (last stack) (last (init stack))], state)
  | show xi == "Neg" =
    run (xf, init stack ++ [negOperation (last stack)], state)
  | otherwise = error "Error in Run"

-- To help you test your assembler
testAssembler :: Code -> IO (String, String)
testAssembler code = do
  (_, stack, state) <- run (code, createEmptyStack, createEmptyState)
  return (stack2Str stack, state2Str state)

-- Examples:
-- yes : testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- yes : testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- yes : testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- yes : testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- yes : testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

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