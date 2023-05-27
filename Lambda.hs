{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lambda where

import Expr
import Data.List (union, delete, (\\))

--- 1.1. find free variables of a Expr ---

free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x e) = x `delete` free_vars e
free_vars (Application e1 e2) = free_vars e2 `union` free_vars e1
free_vars (Macro x) = [x]

--- 1.2. reduce a redex ---

reduce :: Expr -> String -> Expr -> Expr
reduce expr@(Variable x) y e
    | x == y = e
    | otherwise = expr
reduce expr@(Function x e) y e'
    | x == y = expr
    | x `notElem` free_vars e' = Function x (reduce e y e')
    | otherwise = let z = findUnusedVar $ free_vars e `union` free_vars e'
                  in reduce (Function z (reduce e x (Variable z))) y e'
reduce (Application e1 e2) x e = Application (reduce e1 x e) (reduce e2 x e)
reduce expr@(Macro _) _ _ = expr

--- find unused variable ---

findUnusedVar :: [String] -> String
findUnusedVar usedVars = head (vars \\ usedVars)
    where vars = [c : cs | cs <- "" : vars, c <- ['a' .. 'z']]

--- Normal Evaluation ---
--- 1.3. perform one step of Normal Evaluation ---

stepN :: Expr -> Expr
stepN expr@(Variable x) = expr
stepN (Function x e) = Function x $ stepN e
stepN (Application e1 e2) =
    case e1 of
        Variable _ -> Application e1 $ stepN e2
        Function x' e1' -> reduce e1' x' e2
        _ -> Application (stepN e1) e2
stepN expr@(Macro _) = expr

--- 1.4. perform Normal Evaluation ---

reduceN :: Expr -> Expr
reduceN e
    | e == stepN e = e
    | otherwise = reduceN $ stepN e

reduceAllN :: Expr -> [Expr]
reduceAllN e
    | e == stepN e = [e]
    | otherwise = e : reduceAllN (stepN e)

--- Applicative Evaluation ---
--- 1.5. perform one step of Applicative Evaluation ---

stepA :: Expr -> Expr
stepA (Function x e1) = Function x $ stepA e1
stepA (Application e1 e2) =
    case e1 of
        Variable _ -> Application e1 $ stepA e2
        Function x' e1' ->
            case e2 of
                Variable _ -> reduce e1' x' e2
                Function _ _ -> reduce e1' x' e2
                _ -> Application (Function x' e1') (stepA e2)
        _ -> Application (stepA e1) e2
stepA var = var

--- 1.6. perform Applicative Evaluation ---

reduceA :: Expr -> Expr
reduceA e
    | e == stepA e = e
    | otherwise = reduceA $ stepA e

reduceAllA :: Expr -> [Expr]
reduceAllA e
    | e == stepA e = [e]
    | otherwise = e : reduceAllA (stepA e)

--- 3.1. make substitutions into a expression with Macros ---

evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros _ expr@(Variable x) = expr
evalMacros context (Function x e) = Function x (evalMacros context e)
evalMacros context (Application e1 e2) = Application (evalMacros context e1) (evalMacros context e2)
evalMacros context expr@(Macro m) = maybe expr (evalMacros context) (lookup m context)

--- 4.1. evaluate code sequence using given strategy ---

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode _ [] = []
evalCode strategy (Evaluate expr : cs) = strategy (evalMacros [] expr) : evalCode strategy cs
evalCode strategy (Assign str expr : cs) = [strategy (evalMacros [(str, expr)] expr') | expr' <- evalCode strategy cs]
