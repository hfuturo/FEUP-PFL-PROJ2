import Data.List (sort)
import Data.List (intercalate)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code  = [Inst]
type Stack = [Inst]
type State = [Inst]

isPush :: Inst -> Bool
isPush (Push _) = True
isPush _ = False

isFetch :: Inst -> Bool
isFetch (Fetch _) = True
isFetch _ = False

isStore :: Inst -> Bool
isStore (Store _) = True
isStore _ = False

pushValue :: Inst -> Integer
pushValue x = case x of
  Push val -> val
  _        -> error "Not a Push instruction"

storeVar :: Inst -> String
storeVar x = case x of
  Store var -> var
  _        -> error "Not a State instruction"

-- deal with stack
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (stack2Str_aux stack)

stack2Str_aux :: Stack -> [String]
stack2Str_aux [] = []
stack2Str_aux (x:xs) 
  | isPush x = (stack2Str_aux xs) ++ [show (pushValue x)]
  | show(x) == "Fals" = (stack2Str_aux xs) ++ ["False"]
  | show(x) == "Tru" = (stack2Str_aux xs) ++ ["True"]
  | otherwise = error "Do nothing"

-- deal with state
createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," $ map (\(x, y) -> x ++ "=" ++ y) (sort (state2Str_aux state []))

state2Str_aux :: State -> [String] -> [(String,String)]
state2Str_aux [] _ = []
state2Str_aux (x:xs) stack
  | isPush x = state2Str_aux xs (stack ++ [show (pushValue x)])
  | isStore x = (state2Str_aux xs (init stack)) ++ [((storeVar x),(last stack))]
  | show (x) == "Fals" = state2Str_aux xs (stack ++ ["False"])
  | show (x) == "Tru" = state2Str_aux xs (stack ++ ["True"])
  | otherwise = error "Do nothing"

run :: (Code, Stack, State) -> IO (Code, Stack, State)
run ([], stack, state) = return ([], stack, state)
run ((xi:xf), stack, state)
  | isPush xi || show xi == "Fals" || show xi == "Tru" =
      run (xf, stack ++ [xi], state ++ [xi])
  | isStore xi =
    run (xf, init stack, state ++ [xi])
  | show xi == "Add"  =
    run (xf, (take (length stack - 2) stack) ++ [Push ((pushValue (last stack)) + (pushValue (last (init stack))))], state)
  | show xi == "Sub"  =
    run (xf, (take (length stack - 2) stack) ++ [Push ((pushValue (last stack)) - (pushValue (last (init stack))))], state)
  | show xi == "Mult" =
    run (xf, (take (length stack - 2) stack) ++ [Push ((pushValue (last stack)) * (pushValue (last (init stack))))], state)
  | otherwise = error "Error in Run"



-- To help you test your assembler
testAssembler :: Code -> IO (String, String)
testAssembler code = do
  (_, stack, state) <- run (code, createEmptyStack, createEmptyState)
  return (stack2Str stack, state2Str state)

-- Examples:
-- yes : testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- yes : testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
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