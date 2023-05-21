{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lambda where

import Expr
import Data.List
import Control.Monad
import Debug.Trace

-- 1.1. find free variables of a Expr

free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x e) = delete x (free_vars e)
free_vars (Application e1 e2) = free_vars e1 `union` free_vars e2
free_vars (Macro m) = [m]

-- 1.2. reduce a redex

reduce :: Expr -> String -> Expr -> Expr
reduce (Variable y) x e2 = if y == x then e2 else Variable y
reduce (Function y e) x e2
    | x == y = Function y e
    | x /= y && (y `notElem` free_vars e2) = Function y (reduce e x e2)
    | otherwise =
        let z = head (ws \\ (free_vars e `union` free_vars e2))
            ws = do
                n <- [1 ..]
                replicateM n ['a' .. 'z']
        in reduce (Function z (reduce e y (Variable z))) x e2
reduce (Application e1 e2) x e = Application (reduce e1 x e) (reduce e2 x e)
reduce (Macro m) x e2 = Macro m

-- Normal Evaluation
-- 1.3. perform one step of Normal Evaluation

stepN :: Expr -> Expr
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2) =
    if e1 == stepN e1 then Application e1 $ stepN e2
    else Application (stepN e1) e2
stepN f@(Function x e) = Function x $ stepN e
stepN (Macro m) = Macro m
stepN v = v

-- 1.4. perform Normal Evaluation

reduceN :: Expr -> Expr
reduceN e =
    if e == stepN e then e else reduceN (stepN e)

reduceAllN :: Expr -> [Expr]
reduceAllN e =
    if isValue e then [e] else e : reduceAllN (stepN e)

isValue :: Expr -> Bool
isValue (Variable _) = True
isValue (Function _ _) = True
isValue _ = False

-- Applicative Evaluation
-- 1.5. perform one step of Applicative Evaluation

replace :: String -> Expr -> Expr -> Expr
replace x e (Variable y) =
    if x == y then e else Variable y
replace x e (Function y body) =
    if x == y then Function y body else Function y (replace x e body)
replace x e (Application e1 e2) =
    Application (replace x e e1) (replace x e e2)
replace x e (Macro m) = Macro m

stepA :: Expr -> Expr
stepA (Function x e1) = Function x $ stepA e1
stepA (Application (Function x e1) e2) =
    if isValue e2 then replace x e2 e1 else Application (Function x e1) (stepA e2)
stepA (Application e1 e2) =
    if isValue e1 then Application e1 (stepA e2) else Application (stepA e1) e2
stepA (Macro m) = Macro m
stepA e = e

-- 1.6. perform Applicative Evaluation

reduceA :: Expr -> Expr
reduceA e =
    if e == stepA e then e else reduceA (stepA e)

reduceAllA :: Expr -> [Expr]
reduceAllA e =
    if isValue e then [e] else e : reduceAllA (stepA e)

-- 3.1. make substitutions into a expression with Macros

evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros context (Variable x) =
    Variable x
evalMacros context (Function x e) =
    Function x (evalMacros context e)
evalMacros context (Application e1 e2) =
    Application (evalMacros context e1) (evalMacros context e2)
evalMacros context (Macro m) = case lookup m context of
    Just e -> evalMacros context e
    Nothing -> Macro m

-- 4.1. evaluate code sequence using given strategy

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode _ [] = []
evalCode strategy (Assign x e : cs) =
    let updatedStrategy e' = if e' == Macro x then e else strategy e'
    in evalCode updatedStrategy cs
evalCode strategy (Evaluate e : cs) =
    let evaluatedExpr = strategy (aux strategy e)
    in evaluatedExpr : evalCode strategy cs
        where aux :: (Expr -> Expr) -> Expr -> Expr
              aux strategy expr = case expr of
                Variable x -> strategy (Variable x)
                Function x e -> Function x (aux strategy e)
                Application e1 e2 -> Application (aux strategy e1) (aux strategy e2)
                Macro m -> strategy (Macro m)
