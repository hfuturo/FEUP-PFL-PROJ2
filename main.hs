data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | State String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code  = [Inst]
type Stack = [Inst]
type State = [Inst]

isPush :: Inst -> Bool
isPush (Push _) = True
isPush _ = False

isState :: Inst -> Bool
isState (State _) = True
isState _ = False

isFals :: Inst -> Bool
isFals Fals = True
isFals _ = False

isTru :: Inst -> Bool
isTru Tru = True
isTru _ = False

pushValue :: Inst -> Integer
pushValue x = case x of
  Push val -> val
  _        -> error "Not a Push instruction"

stateVar :: Inst -> String
stateVar x = case x of
  State var -> var
  _        -> error "Not a State instruction"

-- deal with stack
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = show x
stack2Str (x:xs) = (stack2Str xs) ++ "," ++ (show x)

-- deal with state
createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = init (state2Str_aux state [])

state2Str_aux :: State -> [String] -> String
state2Str_aux [] _ = ""
state2Str_aux (x:xs) stack
  | isPush x = state2Str_aux xs (stack ++ [show (pushValue x)])
  | isFals x = state2Str_aux xs (stack ++ ["False"])
  | isTru x = state2Str_aux xs (stack ++ ["True"])
  | isState x = (stateVar x) ++ "=" ++ (last stack) ++ "," ++ state2Str_aux xs (init stack)
  | otherwise = error "Error in state2Str_aux (x:xs) stack"

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

-- To help you test your assembler
-- testAssembler :: Code -> (String, String)
-- testAssembler code = (stack2Str stack, state2Str state)
--  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,State "var",State "a", State "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,State "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,State "x",Push 1,Fetch "x",Sub,State "x"] == ("","x=4")
-- testAssembler [Push 10,State "i",Push 1,State "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,State "fact",Push 1,Fetch "i",Sub,State "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,State "y", Fetch "x",Tru]
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
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")