module Exp where

data Aexp = "Add" | "Sub" | "Mult" deriving Show
data Bexp = "==" | "<=" | ">=" deriving Show
data Stm = "if" | "then" | "else" | ":=" deriving Show