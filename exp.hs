module Exp where

data Aexp = 
    Add Aexp Aexp | Sub Aexp Aexp | Mult Aexp Aexp | Neg Aexp deriving Show
data Bexp = Equ Bexp Bexp | Leq Bexp Bexp | Neg Bexp deriving Show
data Stm = "if" | "then" | "else" | ":=" deriving Show