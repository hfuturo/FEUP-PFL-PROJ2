import Data.List (sort, intercalate)
import Inst
import Action
import Lexer
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
data Aexp = 
  Addexp Aexp Aexp  | Subexp Aexp Aexp | 
  Multexp Aexp Aexp | IntVarexp Integer |
  StringVarexp String 
  deriving (Show)

data Stm = 
  Storexp String Aexp deriving (Show)

compA :: Aexp -> Code
compA (IntVarexp n) = [Push n]
compA (StringVarexp n) = [Fetch n]
compA (Addexp e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (Subexp e1 e2) = compA e1 ++ compA e2 ++ [Sub]
compA (Multexp e1 e2) = compA e1 ++ compA e2 ++ [Mult]

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined  -- TODO

parseVarPar :: [Token] -> Maybe (Aexp, [Token])
parseVarPar (IntTok n : restTokens) = Just (IntVarexp n, restTokens)
parseVarPar (VarTok n : restTokens) = Just (StringVarexp n, restTokens)
parseVarPar (OpenTok : restTokens1) =
  case parseAddSub restTokens1 of
    Just (expr, (CloseTok : restTokens2)) ->
      Just (expr, restTokens2)
    _ -> Nothing -- no closing paren
parseVarPar _ = Nothing

parseMult :: [Token] -> Maybe (Aexp, [Token])
parseMult tokens
  = case parseVarPar tokens of
    Just (expr1, (MultTok : restTokens1)) ->
      case parseMult restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Multexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseAddSub::[Token] -> Maybe (Aexp, [Token])
parseAddSub tokens
  = case parseMult tokens of
    -- if +
    Just (expr1, (PlusTok : restTokens1)) ->
      case parseAddSub restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Addexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    -- if -
    Just (expr1, (SubTok : restTokens1)) ->
      case parseAddSub restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Subexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseAexp :: [Token] -> Aexp
parseAexp tokens =
  case parseAddSub tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error"

nextComaPointIndex :: [Token] -> Int
nextComaPointIndex [] = 2
nextComaPointIndex [ComaPointTok] = 0
nextComaPointIndex (ComaPointTok: _) = 0
nextComaPointIndex (x:xs) = 1 + nextComaPointIndex xs

parseSmt :: [Token] -> [Aexp]
parseSmt [] = []
parseSmt tokens
  | index == 0 = []
  | length tokens >= index = [parseAexp (take index tokens)] ++ (parseSmt (tail (drop index tokens)))
  | otherwise = error "Error in parse"
  where 
    index = nextComaPointIndex tokens

--parse :: [Token] -> [Stm]
--parse

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, store2Str store)
  --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)