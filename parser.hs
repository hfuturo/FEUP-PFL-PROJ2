module Parser where
import Lexer
import Debug.Trace

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
  | VarBexp String
  | Leexp Aexp Aexp
  | EqAexp Aexp Aexp
  | EqBexo Bexp Bexp
  | Notexp Bexp
  | Andexp Bexp Bexp
  deriving (Show)

data Stm 
  = Storexp String Aexp 
  | StorBexp String Bexp
  | Ifexp Bexp [Stm] [Stm]
  | Loopexp Bexp [Stm]
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
--parseB ((VarTok n):restTokens) = Just (VarBexp n, restTokens)
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
      Just ((StringVarexp var), (EquTok : restTokens1))
        -> Just (VarBexp var,(EquTok : restTokens1))
      Just ((StringVarexp var), (AndTok : restTokens1))
        -> Just (VarBexp var,(AndTok : restTokens1))
      Just ((StringVarexp var),[])
        -> Just (VarBexp var,[])

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
indexTok :: [Token] -> Token -> Int -> Int
indexTok (OpenTok:xs) token number = 1 + (indexTok xs token (number + 1))
indexTok (CloseTok:xs) token number = 1 + (indexTok xs token (number - 1))
indexTok (ElseTok:_) ElseTok 0 = 0
indexTok (ThenTok:_) ThenTok 0 = 0
indexTok (DoTok:_) DoTok 0 = 0
indexTok (x:xs) token number = 1 + (indexTok xs token number)

endIndex :: [Token] -> Int -> Int
endIndex (OpenTok:xs) number = 1 + (endIndex xs (number + 1))
endIndex (CloseTok:xs) number = 1 + (endIndex xs (number - 1))
endIndex (ComaPointTok:xs) number 
  | number == 0 = 1
  | otherwise = 1 + (endIndex xs number)
endIndex (x:xs) number = 1 + (endIndex xs number)

checkBool :: [Token] -> Bool
checkBool [] = False
checkBool (TrueTok:_) = True
checkBool (FalseTok:_) = True
checkBool (LessEquTok:_) = True
checkBool (DoubleEquTok:_) = True
checkBool (x:xs) = checkBool xs

parseSmt :: [Token] -> Program
parseSmt [] = []
parseSmt (VarTok n : PointEquTok : restToken)
  | index == 0 = []
  | length restToken >= index && (checkBool (take index restToken)) == False = [Storexp n (parseAexp (take index restToken))] ++ parseSmt (tail (drop index restToken))
  | length restToken >= index && (checkBool (take index restToken)) == True = [StorBexp n (parseBexp (take index restToken))] ++ parseSmt (tail (drop index restToken))
  | otherwise = error "Error in parse"
  where index = nextComaPointIndex restToken

parseSmt (IfTok : restToken)
  | indexThen == 0 = []
  | length restToken >= indexThen = [Ifexp (parseBexp expThen) (parseSmt expElse) (parseSmt expEnd)] ++ parseSmt ((drop (indexThen + indexElse + indexComa + 2) restToken))
  | otherwise = error "Error in parse"
  where
    indexThen = indexTok restToken ThenTok 0
    expThen = take indexThen restToken
    indexElse = indexTok (tail (drop indexThen restToken)) ElseTok 0
    expElse = take indexElse (drop indexThen restToken)
    indexComa = endIndex (tail (drop indexElse (tail (drop indexThen restToken)))) 0
    expEnd = take indexComa (drop indexElse (tail (drop indexThen restToken)))

parseSmt (WhileTok : restToken)
  | indexDo == 0 = []
  | length restToken >= indexDo = [Loopexp (parseBexp expDo) (parseSmt expEnd)] -- ++ parseSmt (drop (indexDo + indexComa + 1) restToken)
  | otherwise = error "Error in parse"
  where
    indexDo = indexTok restToken DoTok 0
    expDo = take indexDo restToken
    indexComa = endIndex (tail (drop indexDo restToken)) 0
    expEnd = take indexComa (drop indexDo restToken)

parseSmt (ElseTok : OpenTok : restToken) = parseSmt (init restToken) -- tira )
parseSmt (ThenTok : OpenTok : restToken) = parseSmt restToken
parseSmt (DoTok : OpenTok : restToken) = parseSmt (init restToken) -- tira )
parseSmt (ElseTok : restToken) = parseSmt (restToken ++ [ComaPointTok])
parseSmt (ThenTok : restToken) = parseSmt (restToken ++ [ComaPointTok])

parse :: String -> Program
parse [] = []
parse text = parseSmt (lexer text)