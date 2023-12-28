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
    deriving (Show)

type Program = [Stm]

nextComaPointIndex :: [Token] -> Int
nextComaPointIndex [] = 2
nextComaPointIndex [ComaPointTok] = 0
nextComaPointIndex (ComaPointTok: _) = 0
nextComaPointIndex (x:xs) = 1 + nextComaPointIndex xs

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

parseSmt :: [Token] -> Program
parseSmt [] = []
parseSmt (VarTok n : PointEquTok : restToken)
  | index == 0 = []
  | length restToken >= index = [Storexp n (parseAexp (take index restToken))] ++ parseSmt (tail (drop index restToken))
  | otherwise = error "Error in parse"
  where index = nextComaPointIndex restToken

parse :: String -> Program
parse [] = []
parse text = parseSmt (lexer text)