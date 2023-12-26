module Inst where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Value = Either Integer String

type Code  = [Inst]
type Stack = [Value]
type State = [(String,Value)]

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
