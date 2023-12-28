module Parser where
import Lexer

data Aexp 
    = Addexp Aexp Aexp
    | Subexp Aexp Aexp 
    | Multexp Aexp Aexp
    | IntVarexp Integer
    | StringVarexp String 
    deriving (Show)

data Bexp
  = Trueexp
  | Falseexp
  | Leexp Aexp Aexp
  | EqAexp Aexp Aexp
  | EqBexo Bexp Bexp
  | Notexp Bexp
  | Andexp Bexp Bexp
  deriving (Show)

data Stm 
  = Storexp String Aexp 
  | Ifexp Bexp [Stm] [Stm]
  deriving (Show)

type Program = [Stm]

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

parseB :: [Token] -> Maybe (Bexp, [Token])
parseB (TrueTok : restTokens) = Just (Trueexp, restTokens)
parseB (FalseTok : restTokens) = Just (Falseexp, restTokens)
parseB (OpenTok : restTokens1) =
  case parseB2 restTokens1 of
    Just (expr, (CloseTok : restTokens2)) ->
      Just (expr, restTokens2)
    _ -> Nothing -- no closing paren
parseB tokens
  = case parseAddSub tokens of
      Just (expr1, (LessEquTok : restTokens1)) ->
        case parseAddSub restTokens1 of
          Just (expr2, restTokens2) ->
            Just (Leexp expr1 expr2, restTokens2)
          Nothing -> Nothing
      Just (expr1, (DoubleEquTok : restTokens1)) ->
        case parseAddSub restTokens1 of
          Just (expr2, restTokens2) ->
            Just (EqAexp expr1 expr2, restTokens2)
          Nothing -> Nothing

parseB1 :: [Token] -> Maybe (Bexp, [Token])
parseB1 (NotTok : restTokens1) =
  case parseB restTokens1 of
    Just (expr, restTokens2) ->
      Just (Notexp expr, restTokens2)
    result -> result
parseB1 tokens = parseB tokens


parseB2 :: [Token] -> Maybe (Bexp, [Token])
parseB2 tokens = 
  case parseB1 tokens of
    Just (expr1, (EquTok : restTokens1)) ->
      case parseB2 restTokens1 of
        Just (expr2, restTokens2) ->
          Just (EqBexo expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (AndTok : restTokens1)) ->
      case parseB2 restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Andexp expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseBexp :: [Token] -> Bexp
parseBexp tokens =
  case parseB2 tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error"

nextComaPointIndex :: [Token] -> Int
nextComaPointIndex [] = 2
nextComaPointIndex [ComaPointTok] = 0
nextComaPointIndex (ComaPointTok: _) = 0
nextComaPointIndex (x:xs) = 1 + nextComaPointIndex xs

-- if and else
elseIndex :: [Token] -> Int -> Int
elseIndex (OpenTok:xs) number = 1 + (elseIndex xs (number + 1))
elseIndex (CloseTok:xs) number = 1 + (elseIndex xs (number - 1))
elseIndex (ElseTok:xs) number 
  | number == 0 = 0
  | otherwise = 1 + (elseIndex xs number)
elseIndex (x:xs) number = 1 + (elseIndex xs number)

thenIndex :: [Token] -> Int -> Int
thenIndex (OpenTok:xs) number = 1 + (thenIndex xs (number + 1))
thenIndex (CloseTok:xs) number = 1 + (thenIndex xs (number - 1))
thenIndex (ThenTok:xs) number 
  | number == 0 = 0
  | otherwise = 1 + (thenIndex xs number)
thenIndex (x:xs) number = 1 + (thenIndex xs number)

endIndex :: [Token] -> Int -> Int
endIndex (OpenTok:xs) number = 1 + (endIndex xs (number + 1))
endIndex (CloseTok:xs) number = 1 + (endIndex xs (number - 1))
endIndex (ComaPointTok:xs) number 
  | number == 0 = 1
  | otherwise = 1 + (endIndex xs number)
endIndex (x:xs) number = 1 + (endIndex xs number)

parseSmt :: [Token] -> Program
parseSmt [] = []
parseSmt (VarTok n : PointEquTok : restToken)
  | index == 0 = []
  | length restToken >= index = [Storexp n (parseAexp (take index restToken))] ++ parseSmt (tail (drop index restToken))
  | otherwise = error "Error in parse"
  where index = nextComaPointIndex restToken

parseSmt (IfTok : restToken)
  | indexThen == 0 = []
  | length restToken >= indexThen = [Ifexp (parseBexp expThen) (parseSmt expElse) (parseSmt expEnd)] ++ parseSmt ((drop (indexThen + indexElse + indexComa + 2) restToken))
  | otherwise = error "Error in parse"
  where
    indexThen = thenIndex restToken 0
    expThen = take indexThen restToken
    indexElse = elseIndex (tail (drop indexThen restToken)) 0
    expElse = take indexElse (drop indexThen restToken)
    indexComa = endIndex (tail (drop indexElse (tail (drop indexThen restToken)))) 0
    expEnd = take indexComa (drop indexElse (tail (drop indexThen restToken)))

parseSmt (ElseTok : OpenTok : restToken) = parseSmt (init restToken) -- tira )
parseSmt (ThenTok : OpenTok : restToken) = parseSmt restToken
parseSmt (ElseTok : restToken) = parseSmt (restToken ++ [ComaPointTok])
parseSmt (ThenTok : restToken) = parseSmt (restToken ++ [ComaPointTok])

parse :: String -> Program
parse [] = []
parse text = parseSmt (lexer text)