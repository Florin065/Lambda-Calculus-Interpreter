{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lambda where

import Expr
import Data.List

-- 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars e
  = case e of
      (Variable x) -> [x]
      (Function x e) -> delete x (free_vars e)
      (Application e1 e2) -> free_vars e1 `union` free_vars e2

-- 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable y) x e_2
  | y == x = e_2
  | otherwise = Variable y
reduce (Function y e) x e_2
  | y == x = Function y e
  | y `elem` freeVariables e_2 = let y' = findFreshVariable y (freeVariables e_2)
                                in Function y' (reduce (replaceVariable e y (Variable y')) x e_2)
  | otherwise = Function y (reduce e x e_2)
reduce (Application e1 e2) x e_2 = Application (reduce e1 x e_2) (reduce e2 x e_2)

replaceVariable :: Expr -> String -> Expr -> Expr
replaceVariable (Variable y) x e
  | y == x = e
  | otherwise = Variable y
replaceVariable (Function y e1) x e2
  | y == x = Function y e1
  | otherwise = Function y (replaceVariable e1 x e2)
replaceVariable (Application e1 e2) x e = Application (replaceVariable e1 x e) (replaceVariable e2 x e)

freeVariables :: Expr -> [String]
freeVariables (Variable y) = [y]
freeVariables (Function y e) = filter (/= y) (freeVariables e)
freeVariables (Application e1 e2) = freeVariables e1 ++ freeVariables e2

findFreshVariable :: String -> [String] -> String
findFreshVariable x existingVars
  | x `elem` existingVars = findFreshVariable (x ++ "'") existingVars
  | otherwise = x

-- Normal Evaluation
-- 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable x) = Variable x
stepN (Function x e) = Function x (stepN e)
stepN (Application (Function x e) e2) = replaceVariable e x e2
stepN (Application e1 e2) = Application (stepN e1) e2

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN = undefined

reduceAllN :: Expr -> [Expr]
reduceAllN = undefined

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA = undefined

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA = undefined

reduceAllA :: Expr -> [Expr]
reduceAllA = undefined

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
