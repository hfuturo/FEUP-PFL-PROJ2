module Action where
import Inst

addOperation :: Value -> Value -> Value
addOperation (Left a) (Left b) = Left (a+b)
addOperation _ _ = error "Error in addOperation"

subOperation :: Value -> Value -> Value
subOperation (Left a) (Left b) = Left (a-b)
subOperation _ _ = error "Error in subOperation"

multOperation :: Value -> Value -> Value
multOperation (Left a) (Left b) = Left (a*b)
multOperation _ _ = error "Error in multOperation"

negOperation :: Value -> Value
negOperation (Right "ff") = Right "tt"
negOperation (Right "tt") = Right "ff"
negOperation (Left a) = Left (-a)

fetchOperation :: State -> String -> Value
fetchOperation [] string = error "Error in fetchOperation"
fetchOperation ((a,b):xs) string
    | a == string = b
    | otherwise = fetchOperation xs string

equOperation :: Value -> Value -> Value
equOperation (Left a) (Left b) 
    | a == b = Right "tt"
    | otherwise = Right "ff"

equOperation (Right a) (Right b)
    | a == b = Right "tt"
    | otherwise = Right "ff"

equOperation _ _ = Right "ff"

leOperation :: Value -> Value -> Value
leOperation (Left a) (Left b) 
    | a /= b = Right "tt"
    | otherwise = Right "ff"

leOperation (Right a) (Right b)
    | a /= b = Right "tt"
    | otherwise = Right "ff"

leOperation _ _ = Right "tt"
