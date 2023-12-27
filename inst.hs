module Inst where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Value = Either Integer String

type Code  = [Inst]
type Stack = [Value]
type State = [(String,Value)]

isLoop :: Inst -> Bool
isLoop (Loop _ _) = True
isLoop _ = False

isBranch :: Inst -> Bool
isBranch (Branch _ _) = True
isBranch _ = False

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

fetchVar :: Inst -> String
fetchVar x = case x of
  Fetch var -> var
  _        -> error "Not a State instruction"

loopC1Var :: Inst -> Code
loopC1Var x = case x of
    Loop c1 _ -> c1
    _        -> error "Not a Loop c1 instruction"

loopC2Var :: Inst -> Code
loopC2Var x = case x of
    Loop _ c2 -> c2
    _        -> error "Not a Loop c2 instruction"

branchC1Var :: Inst -> Code
branchC1Var x = case x of
    Branch c1 _ -> c1
    _        -> error "Not a Loop c1 instruction"

branchC2Var :: Inst -> Code
branchC2Var x = case x of
    Branch _ c2 -> c2
    _        -> error "Not a Loop c2 instruction"

-- part 2 --

data Token = 
  PlusTok    | MultTok      | SubTok           | OpenTok | CloseTok   | 
  IntTok Integer | ComaTok      | ComaPointTok | MoreTok     | LessTok | MoreEquTok | 
  LessEquTok | DoubleEquTok | EquTok       | PointEquTok | IfTok   | ElseTok    |
  ThenTok    | WhileTok     | TrueTok      | FalseTok    | Var String deriving (Show)

data Aexp = Addexp Aexp Aexp | Subexp Aexp Aexp | Multexp Aexp Aexp | IntVarexp Integer deriving (Show)
-- data Smt = Aexp
-- type Program = Aexp
