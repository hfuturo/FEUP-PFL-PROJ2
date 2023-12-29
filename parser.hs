module Parser where
import Lexer
import Debug.Trace

-- Data type that represents an arithmetic expression
data Aexp
    = AddAexp Aexp Aexp     -- Add
    | SubAexp Aexp Aexp     -- Subtraction
    | MultAexp Aexp Aexp    -- Multiply
    | IntAexp Integer       -- Numbers
    | VarAexp String        -- Variable
    deriving (Show)

-- Data type that represents a boolean expression
data Bexp
  = TrueBexp            -- True
  | FalseBexp           -- False
  | VarBexp String      -- Variable
  | LeBexp Aexp Aexp    -- <=
  | EqABexp Aexp Aexp   -- ==
  | EqBBexp Bexp Bexp   -- =
  | NotBexp Bexp        -- not
  | AndBexp Bexp Bexp   -- and
  deriving (Show)

-- Data type that represents statements
data Stm
  = StoreAStm String Aexp     -- Stores a variable (for arithmetic)
  | StoreBStm String Bexp     -- Stores a variable (for boolean)
  | IfStm Bexp [Stm] [Stm]    -- If condition
  | LoopStm Bexp [Stm]        -- While loop
  deriving (Show)

-- Synonym for a list of Statements
type Program = [Stm]

-- Aexp : deal with variables and parenteses
parseVarPar :: [Token] -> Maybe (Aexp, [Token])
parseVarPar (IntTok n : restTokens) = Just (IntAexp n, restTokens)
parseVarPar (VarTok n : restTokens) = Just (VarAexp n, restTokens)
parseVarPar (OpenTok : restTokens1) =
  case parseAddSub restTokens1 of
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    _ -> Nothing
parseVarPar _ = Nothing

-- Aexp : deal with multiplications
parseMult :: [Token] -> Maybe (Aexp, [Token])
parseMult tokens
  = case parseVarPar tokens of
    Just (expr1, MultTok : restTokens1) ->
      case parseMult restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MultAexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Aexp : deal with adicions and subtrations
parseAddSub::[Token] -> Maybe (Aexp, [Token])
parseAddSub tokens
  = case parseMult tokens of
    -- if +
    Just (expr1, PlusTok : restTokens1) ->
      case parseAddSub restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AddAexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    -- if -
    Just (expr1, SubTok : restTokens1) ->
      case parseAddSub restTokens1 of
        Just (expr2, restTokens2) ->
          Just (SubAexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parses a list of Tokens that represent an arithmetic expressions
parseAexp :: [Token] -> Aexp
parseAexp tokens =
  case parseAddSub tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error"

-- Bexp : deal with bools and parenteses
parseBoolPar :: [Token] -> Maybe (Bexp, [Token])
parseBoolPar (TrueTok : restTokens) = Just (TrueBexp, restTokens)
parseBoolPar (FalseTok : restTokens) = Just (FalseBexp, restTokens)
parseBoolPar (OpenTok : restTokens1)
  | parTok restTokens1 1 =
    case parseAnd restTokens1 of
      Just (expr, CloseTok : restTokens2) ->
        Just (expr, restTokens2)
      _ -> Nothing

-- case that bool has aritmetic expression within
parseBoolPar tokens
  = case parseAddSub tokens of
      Just (expr1, LessEquTok : restTokens1) ->
        case parseAddSub restTokens1 of
          Just (expr2, restTokens2) ->
            Just (LeBexp expr1 expr2, restTokens2)
          Nothing -> Nothing
      Just (expr1, DoubleEquTok : restTokens1) ->
        case parseAddSub restTokens1 of
          Just (expr2, restTokens2) ->
            Just (EqABexp expr1 expr2, restTokens2)
          Nothing -> Nothing
      Just (VarAexp var, EquTok : restTokens1)
        -> Just (VarBexp var,EquTok : restTokens1)
      Just (VarAexp var, AndTok : restTokens1)
        -> Just (VarBexp var,AndTok : restTokens1)
      Just (VarAexp var,[])
        -> Just (VarBexp var,[])

-- Bexp : deal with negations
parseNot :: [Token] -> Maybe (Bexp, [Token])
parseNot (NotTok : restTokens1) =
  case parseBoolPar restTokens1 of
    Just (expr, restTokens2) ->
      Just (NotBexp expr, restTokens2)
    result -> result
parseNot tokens = parseBoolPar tokens

-- Bexp : deal with equal operations
parseEq :: [Token] -> Maybe (Bexp, [Token])
parseEq tokens =
  case parseNot tokens of
    Just (expr1, EquTok : restTokens1) ->
      case parseEq restTokens1 of
        Just (expr2, restTokens2) ->
           Just (EqBBexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Bexp : deal with and operations
parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
  case parseEq tokens of
    Just (expr1, AndTok : restTokens1) ->
      case parseAnd restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AndBexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parses a list of Tokens that represent a boolean expression
parseBexp :: [Token] -> Bexp
parseBexp tokens =
  case parseAnd tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error bexp"

-- search for the next coma to seperate Stm
nextComaPointIndex :: [Token] -> Int
nextComaPointIndex [] = 2
nextComaPointIndex [ComaPointTok] = 0
nextComaPointIndex (ComaPointTok: _) = 0
nextComaPointIndex (x:xs) = 1 + nextComaPointIndex xs

-- search for index of a specific Token in a independent code block (that is why we check the number of "(" and ")")
indexTok :: [Token] -> Token -> Int -> Int
indexTok [] token number = 0
indexTok (OpenTok:xs) token number = 1 + indexTok xs token (number + 1)
indexTok (CloseTok:xs) token number = 1 + indexTok xs token (number - 1)
indexTok (ElseTok:_) ElseTok 0 = 0
indexTok (ThenTok:_) ThenTok 0 = 0
indexTok (DoTok:_) DoTok 0 = 0
indexTok (x:xs) token number = 1 + indexTok xs token number

-- search for the end of the code block in an else block (because there is no specific Token that indicates the end of it)
endIndex :: [Token] -> Int -> Int
endIndex (OpenTok:xs) number = 1 + endIndex xs (number + 1)
endIndex (CloseTok:xs) number = 1 + endIndex xs (number - 1)
endIndex (ComaPointTok:xs) number
  | number == 0 = 1
  | otherwise = 1 + endIndex xs number
endIndex (x:xs) number = 1 + endIndex xs number

-- check if Stm is has a boolean expression within
checkBool :: [Token] -> Bool
checkBool [] = False
checkBool (TrueTok:_) = True
checkBool (FalseTok:_) = True
checkBool (LessEquTok:_) = True
checkBool (DoubleEquTok:_) = True
checkBool (x:xs) = checkBool xs

parTok :: [Token] -> Int -> Bool
parTok [] number = False
parTok (OpenTok:xs) number = parTok xs (number + 1)
parTok (CloseTok:xs) number
  | (number-1) == 0 = False
  | otherwise = parTok xs (number - 1)
parTok (TrueTok:_) number = True
parTok (FalseTok:_) number = True
parTok (LessEquTok:_) number = True
parTok (DoubleEquTok:_) number = True
parTok (x:xs) number = parTok xs number

-- Converts the list of Tokens given by lexer into a list of Stm ready to be used by the compiler
parseStm :: [Token] -> Program
parseStm [] = []
parseStm (VarTok n : PointEquTok : restToken)
  | index == 0 = []
  | length restToken >= index && not (checkBool (take index restToken)) = StoreAStm n (parseAexp (take index restToken)) : parseStm (tail (drop index restToken))
  | length restToken >= index && checkBool (take index restToken) = StoreBStm n (parseBexp (take index restToken)) : parseStm (tail (drop index restToken))
  | otherwise = error "Error in parse"
  where index = nextComaPointIndex restToken

parseStm (IfTok : restToken)
  | indexThen == 0 = []
  | length restToken >= indexThen = IfStm (parseBexp expThen) (parseStm expElse) (parseStm expEnd) : parseStm (drop (indexThen + indexElse + indexComa + 2) restToken)
  | otherwise = error "Error in parse"
  where
    indexThen = indexTok restToken ThenTok 0
    expThen = take indexThen restToken
    indexElse = indexTok (tail (drop indexThen restToken)) ElseTok 0
    expElse = take indexElse (drop indexThen restToken)
    indexComa = endIndex (tail (drop indexElse (tail (drop indexThen restToken)))) 0
    expEnd = take indexComa (drop indexElse (tail (drop indexThen restToken)))

parseStm (WhileTok : restToken)
  | indexDo == 0 = []
  | length restToken >= indexDo = [LoopStm (parseBexp expDo) (parseStm expEnd)] -- ++ parseStm (drop (indexDo + indexComa + 1) restToken)
  | otherwise = error "Error in parse"
  where
    indexDo = indexTok restToken DoTok 0
    expDo = take indexDo restToken
    indexComa = endIndex (tail (drop indexDo restToken)) 0
    expEnd = take indexComa (drop indexDo restToken)

parseStm (ElseTok : OpenTok : restToken) = parseStm (init restToken) -- tira )
parseStm (ThenTok : OpenTok : restToken) = parseStm restToken
parseStm (DoTok : OpenTok : restToken) = parseStm (init restToken) -- tira )
parseStm (ElseTok : restToken) = parseStm (restToken ++ [ComaPointTok])
parseStm (ThenTok : restToken) = parseStm (restToken ++ [ComaPointTok])
parseStm (OpenTok : restToken) = parseStm restToken
parseStm (CloseTok : restToken) = parseStm (tail restToken) -- tira )

-- Parses the input string given by the user
parse :: String -> Program
parse [] = []
parse text = parseStm (lexer text)