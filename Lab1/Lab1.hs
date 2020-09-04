{- Lab 1
   Authors:
   Lab group:
 -}
---------------------------------------------
import MeasureTime

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1 


-- B -------------------------
-- power1 n k raises n to the k:th power
power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 1 _ = 1
power1 n k = product (replicate (fromInteger k) (fromInteger n))

-- C -------------------------
-- power2 n k raises n to the k:th power
power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 1 _ = 1
power2 n k
  | even k = power2 (n*n) (k `div` 2)
  | otherwise = n * (power2 n (k-1))

-- D -------------------------
{- 
I provide two lists of equal length with varying combinations of large 
and small numbers in the function doTests. I want to cover large 
numbers, small numbers, and 0. Separately, I test negative bases and 
exponents, in the function doNegativeTests, to see that errors are thrown.
-}

-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = (power n k) == (power1 n k) 

-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = (power n k) == (power2 n k)

-- Test functions: 
testPowerFunctions :: [Integer] -> [Integer] -> Bool
testPowerFunctions ns ks 
  | (length ns) /= (length ks) = error "Ensure equal length of lists"
testPowerFunctions [] _ = error "No empty lists allowed"
testPowerFunctions [n] [k] = (comparePower1 n k) && (comparePower2 n k)
testPowerFunctions (n:ns) (k:ks) = (comparePower1 n k) && (comparePower2 n k)
  && (testPowerFunctions ns ks)

doTests :: Bool
doTests = testPowerFunctions [0, 0, 1, 1, 2, 999, 10, 50000000, 51, 3] 
                             [0, 1, 0, 1, 10, 999, 10000, 51, 5000, 4]

doNegativeTests :: Bool
doNegativeTests = testPowerFunctions [0, -0, -3, 1000] [-0, 0, 2, -500] && 
                  doNegativeTests'
doNegativeTests' = testPowerFunctions [-0, 0, 3, -1000] [0, -0, -2, 500]

-- Displaying functions:
getHeaders x n = "n" ++ (getSpaces 1 (fromInteger n)) ++ 
  "power" ++ (getSpaces 11111 (fromInteger (power2 x n))) ++
  "power1" ++ (getSpaces 111111 (fromInteger (power2 x n))) ++ "power2"

-- Prints a table of all values of x to the k, with k
-- being all integers from 0 to given n, for defined power functions
table :: Integer -> Integer -> IO ()
table x n = putStr (unlines ([getHeaders x n] ++ getLines x [0..n] n))

-- Get a single line of values separated by spaces. Definitely a hack,
-- but it is only for the string formatting part of the table, to align
-- the columns.
getLine' :: Integer -> Integer -> Integer -> String
getLine' x k n = show k ++ (getSpaces (fromInteger k) (fromInteger n))
  ++ show (power x k) ++ (getSpaces (fromInteger (power2 x k)) 
  (max 11111 (fromInteger (power2 x n)))) ++ show (power1 x k) ++ 
  (getSpaces (fromInteger (power2 x k)) 
  (max 111111 (fromInteger (power2 x n))))  ++ show (power2 x k)

-- Get a list of lines of numbers for the table
getLines :: Integer -> [Integer] -> Integer -> [String]
getLines x [k] n = (getLine' x k n) : []
getLines x (k:ks) n = [(getLine' x k n)] ++ getLines x ks n 

-- return a string of missing spaces to make up space in a line
getSpaces :: Int -> Int -> String
getSpaces x y = case compare x y of 
 LT -> replicate (length (show y) - length (show x) + 1) ' '
 EQ -> " "
 GT -> " "

