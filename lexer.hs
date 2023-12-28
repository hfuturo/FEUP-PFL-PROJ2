module Lexer where
import Data.Char (isDigit, digitToInt, isAlpha)

data Token 
  = PlusTok        -- +
  | MultTok        -- *
  | SubTok         -- -
  | OpenTok        -- (
  | CloseTok       -- )
  | IntTok Integer -- Int
  | ComaTok        -- ,
  | ComaPointTok   -- ;
  | MoreTok        -- >
  | LessTok        -- <
  | MoreEquTok     -- >=
  | LessEquTok     -- <=
  | DoubleEquTok   -- ==
  | EquTok         -- =
  | PointEquTok    -- :=
  | IfTok          -- if
  | ElseTok        -- else
  | ThenTok        -- then
  | WhileTok       -- while
  | TrueTok        -- true
  | FalseTok       -- false
  | NotTok         -- not
  | AndTok         -- and
  | VarTok String 
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []

lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = SubTok : lexer restStr
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
lexer ('n' : 'o' : 't' : restStr)             = NotTok : lexer restStr
lexer ('a' : 'n' : 'd' : restStr)             = AndTok : lexer restStr


lexer (chr : string)
  | isDigit chr = (IntTok (read digitStr)) : lexer restDigitStr
  | isAlpha chr = (VarTok alphaStr) : lexer restAlphaStr
  | otherwise = error ("Invalid character: " ++ show chr)
  where
    (alphaStr, restAlphaStr) = break (not . isAlpha) (chr : string)
    (digitStr, restDigitStr) = break (not . isDigit) (chr : string)

stringToInt :: String -> Int
stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0  