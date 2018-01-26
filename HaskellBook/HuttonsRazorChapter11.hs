module HuttonsRazor where

data Expr = 
    Lit Integer
  | Add Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

result = eval (Add (Lit 1) (Lit 9001)) 

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

result2 = printExpr (Add (Lit 1) (Lit 9001)) 
-- "1 + 9001" 

a1 = Add (Lit 9001) (Lit 1) 
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2 

result3 = printExpr a3

-- "1 + 9001 + 1 + 20001"

