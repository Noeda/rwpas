module RWPAS.BresenhamLine
  ( bresenhamLine
  , bresenhamLineDirection
  , bresenhamLineDirectionP )
  where

import Control.Monad.State.Strict
import Data.Maybe
import Linear.V2
import RWPAS.Direction

-- | Shoot a line using bresenham's line algorithm.
--
-- Invokes the given function at each point in the line.
--
-- The start point is guaranteed to be included but the target point may or may
-- not be included.
bresenhamLine :: Monad f
              => V2 Int   -- ^ Start here.
              -> V2 Int   -- ^ End here.
              -> (V2 Int -> f Bool) -- ^ Return `False` to stop line shooting.
              -> f (V2 Int)         -- ^ Returns the last point invoked.
bresenhamLine (V2 x1 y1) (V2 x2 y2) fun
  | abs (y2-y1) > abs (x2-x1) = do
      V2 ry rx <- bresenhamLine (V2 y1 x1) (V2 y2 x2)
                                (\(V2 y x) -> fun (V2 x y))
      return (V2 rx ry)
  | x2 < x1 = do
      V2 rx ry <- bresenhamLine (V2 (-x1) y1) (V2 (-x2) y2)
                                (\(V2 x y) -> fun (V2 (-x) y))
      return (V2 (-rx) ry)
  | y2 < y1 = do
      V2 rx ry <- bresenhamLine (V2 x1 (-y1)) (V2 x2 (-y2))
                                (\(V2 x y) -> fun (V2 x (-y)))
      return (V2 rx (-ry))
  | otherwise = bresenhamLineG (V2 x1 y1) (V2 x2 y2) fun

bresenhamLineG :: Monad f
               => V2 Int
               -> V2 Int
               -> (V2 Int -> f Bool)
               -> f (V2 Int)
bresenhamLineG (V2 x1 y1) (V2 x2 y2) fun = do
  let dx = x2-x1
      dy = y2-y1
      d  = 2*dy - dx

  r <- fun (V2 x1 y1)

  if r then loop' (x1+1) y1 d dx dy (V2 x1 y1)
       else return (V2 x1 y1)
 where
  loop' x y d dx dy = loop x y d
   where
    loop x _ _ last | x > x2 = return last
    loop x y d _ =
      if d > 0
        then do r <- fun (V2 x (y+1))
                if r
                  then loop (x+1) (y+1) (d + 2*dy - 2*dx) (V2 x (y+1))
                  else return (V2 x (y+1))
        else do r <- fun (V2 x y)
                if r
                  then loop (x+1) y (d + 2*dy) (V2 x y)
                  else return (V2 x y)

-- | Same as `bresenhamLineDirection` but also includes a coordinate relative
-- to origin with `Direction8`.
bresenhamLineDirectionP :: Monad f
                        => V2 Int
                        -> (Direction8 -> V2 Int -> f Bool)
                        -> f ()
bresenhamLineDirectionP target fun = void $ flip evalStateT (V2 0 0) $
  bresenhamLine (V2 0 0)
                target
                (\new_pos -> do
                   old_pos <- get
                   if new_pos /= (V2 0 0) then do
                     let d = fromMaybe (error "deltaToDirection8: impossible bresenham line step")
                                       (deltaToDirection8 (new_pos - old_pos))
                     result <- lift $ fun d new_pos
                     put new_pos
                     return result
                       else return True)

-- | Same as `bresenhamLine` but instead of using coordinates, uses
-- `Direction8`s.
bresenhamLineDirection :: Monad f
                       => V2 Int  -- ^ Target point, assuming (0, 0) is origin.
                       -> (Direction8 -> f Bool) -- ^ Invoked for each step.
                       -> f ()
bresenhamLineDirection target fun =
  bresenhamLineDirectionP target (\d _ -> fun d)
{-# INLINE bresenhamLineDirection #-}


