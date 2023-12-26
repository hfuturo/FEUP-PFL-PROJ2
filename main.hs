import Data.List ( sort, intercalate )
import Data.Char (isDigit, digitToInt, isAlpha)
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

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

data Token = 
  PlusTok    | MultTok      | SubTok       | AddTok      | OpenTok | CloseTok   | 
  IntTok Int | ComaTok      | ComaPointTok | MoreTok     | LessTok | MoreEquTok | 
  LessEquTok | DoubleEquTok | EquTok       | PointEquTok | IfTok   | ElseTok    |
  ThenTok    | WhileTok     | TrueTok      | FalseTok    | Var String deriving (Show)

lexer :: String -> [Token]
lexer [] = []

lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = SubTok : lexer restStr
lexer ('/' : restStr) = AddTok : lexer restStr
lexer ('*' : restStr) = MultTok : lexer restStr

lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer (',' : restStr) = ComaTok : lexer restStr
lexer (';' : restStr) = ComaPointTok : lexer restStr
lexer (' ' : restStr) = lexer restStr

lexer ('<' : '=' : restStr) = LessEquTok : lexer restStr
lexer ('<' : restStr)       = LessTok : lexer restStr
lexer ('>' : '=' : restStr) = MoreEquTok : lexer restStr
lexer ('>' : restStr)       = MoreTok : lexer restStr
lexer (':' : '=' : restStr) = PointEquTok : lexer restStr
lexer ('=' : '=' : restStr) = DoubleEquTok : lexer restStr
lexer ('=' : restStr)       = EquTok : lexer restStr

lexer ('i' : 'f' : restStr)                   = IfTok : lexer restStr
lexer ('e' : 'l' : 's' : 'e' : restStr)       = ElseTok : lexer restStr
lexer ('t' : 'h' : 'e' : 'n' : restStr)       = ThenTok : lexer restStr
lexer ('w' : 'h' : 'i' : 'l' : 'e' : restStr) = WhileTok : lexer restStr
lexer ('T' : 'r' : 'u' : 'e' : restStr)       = TrueTok : lexer restStr
lexer ('F' : 'a' : 'l' : 's' : 'e' : restStr) = FalseTok : lexer restStr

lexer (chr : string)
  | isDigit chr = (IntTok (stringToInt digitStr)) : lexer restDigitStr
  | isAlpha chr = (Var alphaStr) : lexer restAlphaStr
  | otherwise = error ("Invalid character: " ++ show chr)
  where
    (alphaStr, restAlphaStr) = break (not . isAlpha) (chr : string)
    (digitStr, restDigitStr) = break (not . isDigit) (chr : string)

stringToInt :: String -> Int
stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0  

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, store2Str store)
  --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)