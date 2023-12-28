module Action where

-- Data type that represents an instruction
data Inst 
    = Push Integer          
    | Add 
    | Mult 
    | Sub 
    | Tru 
    | Fals 
    | Equ 
    | Le 
    | And 
    | Neg 
    | Fetch String 
    | Store String 
    | Noop 
    | Branch Code Code 
    | Loop Code Code
    deriving Show

-- Represents a value of the stack
type Value = Either Integer String

-- Represents a list of instructions
type Code  = [Inst]

-- Represents a list of values
type Stack = [Value]

-- Represents a list of pairs with strings and values
type State = [(String,Value)]

-- Adds the top most value with the second topmost value of the stack
addOperation :: Value -> Value -> Value
addOperation (Left a) (Left b) = Left (a+b)
addOperation _ _ = error "Run-time error"

-- Subtracts the top most value with the second topmost value of the stack
subOperation :: Value -> Value -> Value
subOperation (Left a) (Left b) = Left (a-b)
subOperation _ _ = error "Run-time error"

-- Multiplies the top most value with the second topmost value of the stack
multOperation :: Value -> Value -> Value
multOperation (Left a) (Left b) = Left (a*b)
multOperation _ _ = error "Run-time error"

-- Applies the logical negation to a boolean
negOperation :: Value -> Value
negOperation (Right "ff") = Right "tt"
negOperation (Right "tt") = Right "ff"
negOperation _ = error "Run-time error"

-- Executes the and (&&) operation with the two topmost elements
-- They must be booleans otherwise an error will be thrown
andOperation :: Value -> Value -> Value
andOperation (Right a) (Right b)
    | a == "tt" && b == "tt" = Right "tt"
    | otherwise = Right "ff"
andOperation _ _ = error "Run-time error"

-- Gets the value of a variable
fetchOperation :: State -> String -> Value
fetchOperation [] string = error "Run-time error"
fetchOperation ((a,b):xs) string
    | a == string = b
    | otherwise = fetchOperation xs string

-- Checks if the top most value is equal to the second topmost value of the stack
equOperation :: Value -> Value -> Value
equOperation (Left a) (Left b) 
    | a == b = Right "tt"
    | otherwise = Right "ff"
equOperation (Right a) (Right b)
    | a == b = Right "tt"
    | otherwise = Right "ff"
equOperation _ _ = error "Run-time error"

-- Checks if the top most value is less or equal to the second topmost value of the stack
leOperation :: Value -> Value -> Value
leOperation (Left a) (Left b) 
    | a <= b = Right "tt"
    | otherwise = Right "ff"
leOperation _ _ = error "Run-time error"

-- Checks if a variable already exists
lookUpValueState :: State -> String -> Bool
lookUpValueState [] string = False
lookUpValueState ((a,b):xs) string
    | a == string = True
    | otherwise = lookUpValueState xs string

-- If the variable already exists replaces the old value with the new value
-- If it does not exist creates the pair (Variable,Value)
storeAuxOperation :: String -> Value -> String -> Value -> (String, Value)
storeAuxOperation x a string value 
    | x == string = (string,value)
    | otherwise = (x,a)

-- Stores the value of a variable
storeOperation :: State -> String -> Value -> State 
storeOperation state string value
    | lookUpValueState state string = map (\(x, a) -> storeAuxOperation x a string value) state
    | otherwise = state ++ [(string, value)]