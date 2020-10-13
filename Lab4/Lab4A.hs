-- Authors: Emil Logren & Daniel Ericsson
-- Date: 

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

-- Expression either an integer, an operator, or a variable (X) raised to an int
data Expr = Cons Int | Op BinOp Expr Expr | X Int


--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr e = case e of
  X n        -> n >= 0
  Op _ e1 e2 -> prop_Expr e1 && prop_Expr e2
  _            -> True


--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x. 

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
-- Make Expr an instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests

rExpr :: Int -> Gen Expr
rExpr n = frequency [(1, genOp), (1, genExpo), (n, genExpr)]
  where
    genOp = do
      op <- elements [AddOp, MulOp]
      x  <- choose (-1000,1000)
      y  <- choose (-1000,1000)
      return (Op op (Cons x) (Cons y))

    genExpo = do 
      n <- choose (0,10)
      return (X n)

    genExpr = let m = n `div` 4 in do
      op <- elements [AddOp, MulOp]
      e1 <- rExpr m
      e2 <- rExpr m
      return (Op op e1 e2)


instance Arbitrary Expr
  where arbitrary = sized rExpr


--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval x expr = case expr of
  Cons c         -> c
  Op AddOp e1 e2 -> eval x e1 + eval x e2
  Op MulOp e1 e2 -> eval x e1 * eval x e2
  X n            -> x^n
  


--------------------------------------------------------------------------------
-- * A6
-- Define
exprToPoly :: Expr -> Poly
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly expr = case expr of 
  Cons c         -> fromList [c]
  X n            -> fromList (1 : replicate n 0)
  Op AddOp e1 e2 -> exprToPoly e1 + exprToPoly e2 
  Op MulOp e1 e2 -> exprToPoly e1 * exprToPoly e2

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression
prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly x expr = eval x expr == evalPoly x (exprToPoly expr)  

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction, 
polyToExpr :: Poly -> Expr
polyToExpr poly = let constants = toList poly in case constants of
  []     -> Cons 0
  [c]    -> Cons c
  (c:cs) -> Op AddOp (toExpr c (length cs)) (polyToExpr $ fromList cs)
    where
      toExpr c n
        | c == 1    = (X n)
        | otherwise = (Op MulOp (Cons c) (X n))


-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr x poly = evalPoly x poly == eval x (polyToExpr poly)

--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify expr = (polyToExpr . exprToPoly) expr 

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

prop_noJunk = undefined

--------------------------------------------------------------------------------
