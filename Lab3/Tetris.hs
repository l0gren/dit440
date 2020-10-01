-- Authors: Emil Logren & Daniel Ericsson, Group 46

-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s

-- ** B04

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (v,p) well qs) = prop_Shape p && 
                                     shapeSize well == wellSize &&
                                     (not . collision) (Tetris (v,p) well qs)

-- ** B05

-- | Add black walls around a shape
-- uses functions addRows and addColumns in Shapes.hs
addWalls :: Shape -> Shape
addWalls s = (addRows 1 (Just Black) . addRows (-1) (Just Black) .
             addColumns 1 (Just Black) . addColumns (-1) (Just Black)) s

-- ** B06

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls (combine w (place (v,p)))

-- ** B08

-- | Move piece down by 1 for each time tick
tick :: Tetris -> Maybe (Int, Tetris)
tick t = t' where
    t' | collision (move (0,1) t) = dropNewPiece t
       | otherwise                = Just (0, move (0,1) t)

-- ** C01

-- | Check if falling piece has collided with the well or
-- any of its contents
collision :: Tetris -> Bool
collision (Tetris (v,p) w qs) = let s = place (v,p) in
    s `overlaps` w || 
    fst v < 0 || 
    fst (shapeSize s) > fst (shapeSize w) ||
    snd (shapeSize s) > snd (shapeSize w)

-- | Generic function for applying any move to a falling
-- Tetris piece, controlling for collisions
doMove :: (Tetris -> Tetris) -> Tetris -> Tetris
doMove f t = t' where
    t' | collision (f t) = t
       | otherwise       = f t

-- ** B07

-- | Move a piece in the game by vector v1
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v,p) w qs) = Tetris ((vAdd v1 v), p) w qs

-- ** C03

-- | Move piece horizontally, unless a collison occurs
movePiece :: Int -> Tetris -> Tetris
movePiece x t = doMove (move (x,0)) t

-- ** C04

-- | Rotates the falling shape 90° clockwise
rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w qs) = (Tetris (v,rotateShape p) w qs)

-- ** C06

-- | Rotates falling piece 90° clockwise, 
-- unless a collision occurs
rotatePiece :: Tetris -> Tetris
rotatePiece t = doMove (adjust . rotate) t

-- ** C05

-- | Adjusts horizontal position of piece to 
-- allow rotating close to a wall
adjust :: Tetris -> Tetris
adjust (Tetris (v,p) w qs) = move (x,0) t where
    t = (Tetris (v,p) w qs)
    shapeWidth = fst (shapeSize p)
    wellWidth  = fst (shapeSize w)
    pos        = fst v
    x | pos + shapeWidth > wellWidth = 
        wellWidth - (pos + shapeWidth)
      | pos < 0 = pos
      | otherwise = 0  

-- ** C07

-- | Handles the end of the falling piece's journey,
-- and provides the game with a new state of either a
-- new falling piece, or termination of the game
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (v,p) w (q:qs))
    | collision (Tetris (v',q) w' qs)   = Nothing
    | otherwise                         = Just (n, Tetris (v',q) w' qs)
    where
          v'     = startPosition
          (n,w') = clearLines (combine (place (v,p)) w)

-- ** C02 (etc.) 

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris a t 
    | a == Tick || a == MoveDown = tick t
    | a == MoveLeft              = Just (0, movePiece (-1) t)
    | a == MoveRight             = Just (0, movePiece 1 t)
    | a == Rotate                = Just (0, rotatePiece t)
stepTetris _ t = Just (0,t) -- incomplete !!!

-- ** C08

-- | The initial game state with a supply of random shapes
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = map (\r -> (allShapes!!(round (r*6.0)))) rs

-- ** C09

-- | Clears all complete lines from the shape and returns a new shape
-- with all remaining lines left together, shifted down, and the number
-- of lines cleared.
clearLines :: Shape -> (Int, Shape)
clearLines (S rs) = (n, shiftShape (0,n) (S rs')) where
    n   = length rs - length rs'
    rs' = filter (not . isComplete) rs 

-- | Helper function. Checks if a row is complete (no empty squares)
isComplete :: Row -> Bool
isComplete []       = True
isComplete (sq:row) = sq /= Nothing && isComplete row