-- Authors: Emil Logren & Daniel Ericsson, Group 46

-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape (c, r) = S (replicate r (emptyRow c))

emptyRow :: Int -> Row
emptyRow c = replicate c Nothing

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S (r:rs)) = (length r, length (r:rs))  

-- ** A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S rs) = blockCount' (concat rs)

blockCount' [] = 0
blockCount' (r:rs) 
    | r == Nothing = 0 + blockCount' rs
    | otherwise = 1 + blockCount' rs

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S (r:rs)) = length (r:rs) > 0 && length r > 0 && checkRect (length r) rs

-- Check if list of lists is rectangular, being of same width as provided
checkRect :: Int -> [Row] -> Bool
checkRect _ [] = True
checkRect w (r:rs) = length r == w && checkRect w rs

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape s = S (transpose (reverse (rows s)))

-- ** A08
-- | Add empty row. Add from top if n positive, from bottom if n negative
addEmptyRow :: Int -> Shape -> Shape
addEmptyRow n s = let width = fst (shapeSize s)
                      empties = replicate (abs n) (emptyRow width)
                  in case compare n 0 of
                      GT -> S ( empties ++ rows s )
                      LT -> S ( rows s  ++ empties)
                      EQ -> s

-- | Add empty column. Add from left if n positive, from right if n negative
addEmptyColumn :: Int -> Shape -> Shape
addEmptyColumn n s = let empty = replicate (abs n) Nothing in case compare n 0 of 
                      GT -> S ([empty ++ row | row <- rows s])
                      LT -> S ([row ++ empty | row <- rows s])
                      EQ -> s

-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (left, top) s = addEmptyRow top (addEmptyColumn left s)

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (right, bottom) s = addEmptyRow (-bottom) (addEmptyColumn (-right) s)

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (width, height) s = padShape (width', height') s where
    size    = shapeSize s
    width'  = max (width - fst size) 0
    height' = max (height - snd size) 0

-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or (zipWith rowsOverlap (rows s1) (rows s2))

rowsOverlap :: Row -> Row -> Bool
rowsOverlap r1 r2 = or [(x /= Nothing && y /= Nothing) | x <- r1, y <- r2]

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f s1 s2 = S [[f x y | x <- r1, y <- r2] |
                           r1 <- rows s1, r2 <- rows s2]

zipShapeWith' f (S (r1:rs1)) (S (r2:rs2)) = S ([zipWith f r1 r2])

blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith' clash s1 s2  
 where 
  clash :: Square -> Square -> Square
  clash Nothing Nothing = Nothing
  clash Nothing s       = s
  clash s       Nothing = s
  clash (Just c1) (Just c2) = Just Black

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = zipShapeWith combineSquares (padShapeTo (w, h) s1) s2
    where
        (w, h) = (fst (shapeSize s1) + fst (shapeSize s2), 
                  snd (shapeSize s1) + snd (shapeSize s2))
        combineSquares (Just c1) Nothing   = Just c1
        combineSquares Nothing (Just c2)   = Just c2
        combineSquares Nothing Nothing     = Nothing
        combineSquares (Just c1) (Just c2) = error "No overlap allowed"

