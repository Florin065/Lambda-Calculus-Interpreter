{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lambda where

import Expr
import Data.List (union, delete, (\\))

--- 1.1. find free variables of a Expr ---

free_vars :: Expr -> [String]
free_vars (Variable s) = [s]
free_vars (Function s e) = s `delete` free_vars e
free_vars (Application e1 e2) = free_vars e2 `union` free_vars e1
free_vars (Macro m) = [m]

--- 1.2. reduce a redex ---

reduce :: Expr -> String -> Expr -> Expr
reduce expr@(Variable s) s' e
    | s == s' = e
    | otherwise = expr
reduce expr@(Function s e) s' e'
    | s == s' = expr
    | s `notElem` free_vars e' = Function s (reduce e s' e')
    | otherwise =   let z = unusedVars $ free_vars e `union` free_vars e'
                        unusedVars usedVars = head (vars \\ usedVars)
                        vars = [show x | x <- ['a' .. 'z']]
                    in reduce (Function z (reduce e s (Variable z))) s' e'
reduce (Application e1 e2) s e = Application (reduce e1 s e) (reduce e2 s e)
reduce expr@(Macro _) _ _ = expr

--- Normal Evaluation ---
--- 1.3. perform one step of Normal Evaluation ---

stepN :: Expr -> Expr
stepN (Function s e) = Function s $ stepN e
stepN (Application e1 e2) =
    case e1 of
        Variable _ -> Application e1 $ stepN e2
        Function s e -> reduce e s e2
        _ -> Application (stepN e1) e2
stepN expr = expr

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
stepA (Function s e) = Function s $ stepA e
stepA (Application e1 e2) =
    case e1 of
        Variable _ -> Application e1 $ stepA e2
        Function s e ->
            case e2 of
                Variable _ -> reduce e s e2
                Function _ _ -> reduce e s e2
                _ -> Application e1 $ stepA e2
        _ -> Application (stepA e1) e2
stepA expr = expr

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
evalMacros _ expr@(Variable _) = expr
evalMacros context (Function s e) = Function s (evalMacros context e)
evalMacros context (Application e1 e2) = Application (evalMacros context e1) (evalMacros context e2)
evalMacros context expr@(Macro m) = maybe expr (evalMacros context) (lookup m context)

--- 4.1. evaluate code sequence using given strategy ---

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode _ [] = []
evalCode strategy (Evaluate e : xs) = strategy (evalMacros [] e) : evalCode strategy xs
evalCode strategy (Assign s e : xs) = [strategy (evalMacros [(s, e)] e') | e' <- evalCode strategy xs]
