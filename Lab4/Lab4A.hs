-- Authors: Emil Logren & Daniel Ericsson
-- Date: 2020-10-15

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- Expression either an integer, a binary operation on two expressions, 
-- or a variable (X) raised to a non-negative Int
data Expr = Cons Int | Op BinOp Expr Expr | X Int


--------------------------------------------------------------------------------
-- * A2
-- | Ensures no negative exponents occur in expressions
prop_Expr :: Expr -> Bool
prop_Expr e = case e of
  X n        -> n >= 0
  Op _ e1 e2 -> prop_Expr e1 && prop_Expr e2
  _          -> True


--------------------------------------------------------------------------------
-- * A3
-- | Rules for showing an expression, that include eliminating superfluous symbols,
-- and correctly parenthesising according to precedence of evaluation
showExpr :: Expr -> String
showExpr e = case e of 
  Cons c         -> show c
  X n            -> showExpo n
    where
      showExpo n
        | n == 0    = show 1
        | n == 1    = "x"
        | otherwise = "x^" ++ show n      
  Op AddOp e1 e2 -> showExpr e1   ++ "+" ++ showExpr e2
  Op MulOp e1 e2 -> showFactor e1 ++ "*" ++ showFactor e2
    where
      showFactor e@(Op op _ _) = case op of
        AddOp -> "(" ++ showExpr e ++ ")"
        MulOp -> showExpr e
      showFactor e = showExpr e


instance Show Expr where
  show = showExpr

--------------------------------------------------------------------------------
-- * A4
-- | Generates random expressions by combining numerical operations and variable
-- with either addition or multiplication, with a given size n
rExpr :: Int -> Gen Expr
rExpr n = frequency [(1, genOp), (1, genExpo), (n, genExpr)]
  where
    genOp = do
      op <- elements [AddOp, MulOp]
      x  <- choose (-1000,1000)
      y  <- choose (-1000,1000)
      return (Op op (Cons x) (Cons y))

    genExpo = do 
      m <- choose (0,10)
      return (X m)

    genExpr = let m = n `div` 4 in do
      op <- elements [AddOp, MulOp]
      e1 <- rExpr m
      e2 <- rExpr m
      return (Op op e1 e2)

instance Arbitrary Expr
  where arbitrary = sized rExpr

--------------------------------------------------------------------------------
-- * A5
-- Evaluates an expression with a given value x
eval :: Int -> Expr -> Int
eval x expr = case expr of
  Cons c         -> c
  Op AddOp e1 e2 -> eval x e1 + eval x e2
  Op MulOp e1 e2 -> eval x e1 * eval x e2
  X n            -> x^n

--------------------------------------------------------------------------------
-- * A6
-- | Converts an expression into a polynomial by adding and multiplying
-- the polynomials of each component of the expression
exprToPoly :: Expr -> Poly
exprToPoly expr = case expr of 
  Cons c         -> fromList [c]
  X n            -> fromList (1 : replicate n 0)
  Op AddOp e1 e2 -> exprToPoly e1 + exprToPoly e2 
  Op MulOp e1 e2 -> exprToPoly e1 * exprToPoly e2

-- | Ensures that the evaluation of an expression is equal to the evaluation
-- of that expression converted into a polynomial
prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly x expr = eval x expr == evalPoly x (exprToPoly expr)  

--------------------------------------------------------------------------------
-- * A7
-- | Converts a polynomial into an expression by first creating a list of its
-- constants and exponents, and then reducing these into a single expression sans junk
polyToExpr :: Poly -> Expr
polyToExpr poly = let constants = (reverse . toList) poly in
  foldr termToExpr (Cons 0) (zip constants [0..(length constants - 1)])
    where
      -- A term (Int, Int) is a constant and an exponent
      termToExpr :: (Int, Int) -> Expr -> Expr
      termToExpr (0, _) e        = e
      termToExpr (1, n) e        = Op AddOp (X n) e
      termToExpr (c, n) (Cons 0) = Op MulOp (Cons c) (X n)
      termToExpr (c, n) e        = Op AddOp (Op MulOp (Cons c) (X n)) e  

-- | Ensures that the evaluation of a polynomial is equal to
-- the evaluation of that polynomial converted into an expression
prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr x poly = evalPoly x poly == eval x (polyToExpr poly)

--------------------------------------------------------------------------------
-- * A8
-- | Simplifies an expression by converting it to a canonical polynomial
-- and then back to an expression
simplify :: Expr -> Expr
simplify expr = (polyToExpr . exprToPoly) expr 

--------------------------------------------------------------------------------
-- * A9
-- | Ensures no additions by 0, and no multiplications by 0 or 1
prop_noJunk :: Expr -> Bool
prop_noJunk (Op AddOp (Cons c1) _)  = c1 /= 0
prop_noJunk (Op AddOp _ (Cons c2))  = c2 /= 0
prop_noJunk (Op MulOp (Cons c1) _)  = c1 /= 0 && c1 /= 1
prop_noJunk (Op MulOp _ (Cons c2))  = c2 /= 0 && c2 /= 1
prop_noJunk (Op _ e1 e2)            = prop_noJunk e1 && prop_noJunk e2
prop_noJunk _                       = True

--------------------------------------------------------------------------------
