module Hw3 where

import Data.List ((\\), nub, delete)

type Symbol = String
data Expr = Var Symbol
          | Lambda Symbol Expr
          | App Expr Expr

-- Fresh variable generator
freshVars :: [String]
freshVars = [c : show n | n <- [1..], c <- ['a'..'z']]

-- Substitute variable 'var' with expression 'val' in 'expr'
substitute :: Expr -> Symbol -> Expr -> Expr
substitute expr@(Var x) var val
    | x == var = val
    | otherwise = expr
substitute expr@(Lambda x body) var val
    | x == var = expr  -- var is bound in the lambda, no substitution
    | x /= var && x `notElem` freeVars val = Lambda x (substitute body var val)
    | otherwise = let newVar = head (freshVars \\ freeVars val)
                  in Lambda newVar (substitute (substitute body x (Var newVar)) var val)
substitute (App e1 e2) var val = App (substitute e1 var val) (substitute e2 var val)

-- Get the list of free variables in the expression
freeVars :: Expr -> [Symbol]
freeVars (Var x) = [x]
freeVars (Lambda x e) = delete x (freeVars e)
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)

-- Evaluation function: reduce expression to normal form
eval :: Expr -> Expr
eval expr@(Var _) = expr
eval expr@(Lambda x e) = Lambda x (eval e)
eval (App (Lambda x e1) e2) = eval (substitute e1 x e2)
eval (App e1 e2)
    | isLambda e1 = eval (App (eval e1) e2)
    | otherwise = App e1 (eval e2)

-- Check if an expression is a lambda
isLambda :: Expr -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

-- Custom show function to display expressions in λ-calculus notation
instance Show Expr where
    show (Var x) = x
    show (Lambda x e) = "(\\" ++ x ++ "." ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
