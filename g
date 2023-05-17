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
