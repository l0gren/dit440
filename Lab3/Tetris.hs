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
prop_Tetris (Tetris (v,p) well ps) = prop_Shape p && 
                                     shapeSize well == wellSize &&
                                     (not . collision) (Tetris (v,p) well ps)

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

-- ** B07

-- | Move a piece in the game by vector v1
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v,p) w ps) = Tetris ((vAdd v1 v), p) w ps

-- ** B08

-- | Move piece down by 1 for each time tick
tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, t') where
    t' | collision (move (0,1) t) = t 
       | otherwise                = move (0,1) t

-- ** C01

-- | Check if falling piece has collided with the well or
-- any of its contents
collision :: Tetris -> Bool
collision (Tetris (v,p) w s) = let s = place (v,p) in
    s `overlaps` w || 
    fst v < 0 || 
    fst (shapeSize s) > fst (shapeSize w) ||
    snd (shapeSize s) > snd (shapeSize w)


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!

-- ** C03

-- | Move piece horizontally, unless a collison occurs
movePiece :: Int -> Tetris -> Tetris
movePiece x t = t' where 
    t' | collision (move (x,0) t) = t
       | otherwise                = move (x,0) t

-- ** C04

-- | Rotates the falling shape 90° clockwise
rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w ps) = (Tetris (v,rotateShape p) w ps)

-- ** C05

-- | Adjusts horizontal position of piece to 
-- allow rotating close to a wall
adjust :: Tetris -> Tetris
adjust (Tetris (v,p) w ps) = move (x,0) t where
    t = (Tetris (v,p) w ps)
    shapeWidth = fst (shapeSize p)
    wellWidth  = fst (shapeSize w)
    pos        = fst v
    x | pos + shapeWidth > wellWidth = 
        wellWidth - (pos + shapeWidth)
      | pos < 0 = pos
      | otherwise = 0

-- ** C02, C06, 

rotatePiece :: Tetris -> Tetris
rotatePiece t = t' where
    t' | collision ((adjust . rotate) t) = t
       | otherwise = (adjust . rotate) t

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris a t 
    | a == Tick || a == MoveDown = tick t
    | a == MoveLeft              = Just (0, movePiece (-1) t)
    | a == MoveRight             = Just (0, movePiece 1 t)
    | a == Rotate                = Just (0, rotatePiece t)
stepTetris _ t = Just (0,t) -- incomplete !!!
