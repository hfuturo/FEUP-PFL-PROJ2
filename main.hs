import Data.List (sort, intercalate)
import Action
import Parser
import Debug.Trace

valueToStr :: Value -> String
valueToStr (Left x) = show x
valueToStr (Right s) 
  | s == "ff" = show False
  | s == "tt" = show True
  | otherwise = s

-- deal with stack
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map valueToStr (reverse stack)

-- deal with state
createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," $ map (\(x, y) -> x ++ "=" ++ (valueToStr y)) (sort state)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((xi:xf), stack, state) =
    case xi of 
        Branch c1 c2
          | (last stack) == Right "tt" -> run (c1 ++ xf, init stack, state)
          | (last stack) == Right "ff" -> run (c2 ++ xf, init stack, state)
          | otherwise -> error "Run time error"
        Loop c1 c2 -> 
          run (xf ++ c1 ++ [Branch (c2 ++ [xi]) [Noop]], stack, state)
        Push val -> 
          run (xf, stack ++ [Left val], state)
        Noop -> 
          run (xf, stack, state)
        Fals -> 
          run (xf, stack ++ [Right "ff"], state)
        Tru -> 
          run (xf, stack ++ [Right "tt"], state)
        Store var -> 
          run (xf, init stack, storeOperation state var (last stack))
        Fetch var -> 
          run (xf, stack ++ [fetchOperation state var], state)
        Add -> 
          run (xf, take (length stack - 2) stack ++ [addOperation (last stack) (last (init stack))], state)
        Sub -> 
          run (xf, take (length stack - 2) stack ++ [subOperation (last stack) (last (init stack))], state)
        Mult -> 
          run (xf, take (length stack - 2) stack ++ [multOperation (last stack) (last (init stack))], state)
        Neg -> 
          run (xf, init stack ++ [negOperation (last stack)], state)
        And -> 
          run (xf, take (length stack - 2) stack ++ [andOperation (last stack) (last (init stack))], state)
        Equ -> 
          run (xf, take (length stack - 2) stack ++ [(equOperation (last stack) (last (init stack)))],state)
        Le -> 
          run (xf, take (length stack - 2) stack ++ [(leOperation (last stack) (last (init stack)))],state)
        _ -> error "Run-time error"

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Part 2
compA :: Aexp -> Code
compA (IntVarexp n) = [Push n]
compA (StringVarexp n) = [Fetch n]
compA (Addexp e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (Subexp e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (Multexp e1 e2) = compA e2 ++ compA e1 ++ [Mult]

compB :: Bexp -> Code
compB (Falseexp) = [Fals]
compB (Trueexp) = [Tru]
compB (Leexp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (EqAexp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Equ]
compB (EqBexo bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [Equ]
compB (Notexp bexp) = compB bexp ++ [Neg]
compB (Andexp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]

compile :: Program -> Code
compile [] = []
compile ((Storexp var aexp): restProgram) = compA aexp ++ [Store var] ++ compile restProgram
compile ((Ifexp bexp code1 code2): restProgram) = compB bexp ++ [Branch (compile code1) (compile code2)] ++ compile restProgram

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)