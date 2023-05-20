{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lambda where

import Expr
import Data.List
import Control.Monad
import GHC.Float (Floating(expm1))

-- 1.1. find free variables of a Expr

free_vars :: Expr -> [String]
free_vars (Application e1 e2) = free_vars e1 `union` free_vars e2
free_vars (Function x e) = [x' | x' <- free_vars e, x' /= x]
free_vars (Variable x) = [x]
free_vars (Macro m) = []

-- 1.2. reduce a redex

reduce :: Expr -> String -> Expr -> Expr
reduce (Variable x') x e2
    | x' == x = e2
    | otherwise = Variable x'
reduce (Application e1 e2) x e = Application (reduce e1 x e) (reduce e2 x e)
reduce (Function x' e) x e2
    | x == x' = Function x' e
    | x /= x' && (x' `notElem` free_vars e2) = Function x' (reduce e x e2)
    | otherwise =
        let z = head (ws \\ (free_vars e `union` free_vars e2))
            ws = do
                n <- [1 ..]
                replicateM n ['a' .. 'z']
        in reduce (Function z (reduce e x' (Variable z))) x e2
reduce (Macro m) x e2 = Macro m

-- Normal Evaluation
-- 1.3. perform one step of Normal Evaluation

stepN :: Expr -> Expr
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2)
    | e1 == stepN e1 = Application e1 (stepN e2)
    | otherwise = Application (stepN e1) e2
stepN f@(Function x e) -- = Function x (stepN e)
    | e == stepN e = f
    | otherwise = Function x (stepN e)
stepN v = v

-- 1.4. perform Normal Evaluation

reduceN :: Expr -> Expr
reduceN e
    | e == stepN e = e
    | otherwise = reduceN (stepN e)

reduceAllN :: Expr -> [Expr]
reduceAllN e
    | e == stepN e = [e]
    | otherwise = e : reduceAllN (stepN e)

-- Applicative Evaluation
-- 1.5. perform one step of Applicative Evaluation

replace :: String -> Expr -> Expr -> Expr
replace x e (Variable y)
  | x == y = e
  | otherwise = Variable y
replace x e (Function y body)
  | x == y = Function y body
  | otherwise = Function y (replace x e body)
replace x e (Application e1 e2) =
    Application (replace x e e1) (replace x e e2)
replace x e (Macro m) = Macro m

isValue :: Expr -> Bool
isValue (Variable _) = True
isValue (Function _ _) = True
isValue _ = False

stepA :: Expr -> Expr
stepA (Function x e1) = Function x $ stepA e1
stepA (Application (Function x e1) e2)
  | isValue e2 = replace x e2 e1
  | otherwise = Application (Function x e1) (stepA e2)
stepA (Application e1 e2)
  | isValue e1 = Application e1 (stepA e2)
  | otherwise = Application (stepA e1) e2
stepA e = e

-- 1.6. perform Applicative Evaluation

reduceA :: Expr -> Expr
reduceA e
    | e == stepA e = e
    | otherwise = reduceA (stepA e)

reduceAllA :: Expr -> [Expr]
reduceAllA e
    | isValue e = [e]
    | otherwise = e : reduceAllA (stepA e)

-- 3.1. make substitutions into a expression with Macros

evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros context (Variable x) = Variable x
evalMacros context (Function x e) = Function x (evalMacros context e)
evalMacros context (Application e1 e2) =
    Application (evalMacros context e1) (evalMacros context e2)
evalMacros context macro@(Macro m) =
    case lookup m context of
        Just e -> evalMacros context e
        Nothing -> macro

-- 4.1. evaluate code sequence using given strategy

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
