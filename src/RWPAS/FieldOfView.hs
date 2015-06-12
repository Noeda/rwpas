-- | Line of sight, an abstract implementation, implemented so that it can be
-- easily used in portal implementaiton.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module RWPAS.FieldOfView
  ( computeFieldOfView
  , bresenhamLine
  , ByDirection(..)
  , leftD
  , rightD
  , upD
  , downD )
  where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Data
import Data.Foldable
import GHC.Generics

data ByDirection a = ByDirection
  { _leftD :: !a
  , _leftupD :: !a
  , _leftdownD :: !a
  , _upD :: !a
  , _rightD :: !a
  , _uprightD :: !a
  , _downD :: !a
  , _downrightD :: !a }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )
makeLensesFor [("_leftD", "leftD")
              ,("_rightD", "rightD")
              ,("_uprightD", "uprightD")
              ,("_upD", "upD")
              ,("_downD", "downD")
              ,("_downrightD", "downrightD")]
              ''ByDirection

flipX :: ByDirection a -> ByDirection a
flipX (ByDirection{..}) = ByDirection
  { _leftD = _rightD
  , _leftupD = _uprightD
  , _leftdownD = _downrightD
  , _rightD = _leftD
  , _uprightD = _leftupD
  , _downrightD = _leftdownD
  , _upD = _upD
  , _downD = _downD }

rotate90 :: ByDirection a -> ByDirection a
rotate90 (ByDirection{..}) = ByDirection
  -- Rotate clockwise
  { _leftD = _downD
  , _upD = _leftD
  , _rightD = _upD
  , _downD = _rightD

  , _leftupD = _leftdownD
  , _uprightD = _leftupD
  , _downrightD = _uprightD
  , _leftdownD = _downrightD }

-- | Computes the field of view in two dimensions.
computeFieldOfView :: (Monad m, Ord a)
           => (a -> m ()) -- ^ I can see this square. May be called more than once.
           -> (a -> Bool) -- ^ Can you see through this square?
           -> ByDirection (a -> m (Maybe a))  -- ^ Functions that move to some direction. If you use portals, implement that logic in these functions. Return Nothing if this is the boundary of area.
           -> a   -- ^ Start from this square
           -> Int -- ^ How many steps you can go right (0 == the area is just one vertical column)
           -> Int -- ^ How many steps you can go up.
           -> m ()
computeFieldOfView i_see see_through move_functions starting_point x_extent y_extent = do
  -- We can always see the square we are on
  i_see starting_point

  -- cardinal directions
  straight starting_point (move_functions^.leftD) 0
  straight starting_point (move_functions^.rightD) 0
  straight starting_point (move_functions^.upD) 0
  straight starting_point (move_functions^.downD) 0

  -- shoot right
  bresenhamsLineShooter move_functions x_extent y_extent
  -- shoot left
  bresenhamsLineShooter (flipX move_functions) x_extent y_extent
  -- shoot down
  bresenhamsLineShooter (rotate90 move_functions) y_extent x_extent
  -- shoow up
  bresenhamsLineShooter (rotate90 $ rotate90 $ rotate90 move_functions) y_extent x_extent
 where
  -- Here's the idiot algorithm:
  --
  -- Shoot a line to every point in the LOS area edges.
  -- The 'i_see' function will be called for some spot if there exists is a
  -- line that passes through it.
  --
  -- There's a lot of optimize here. Many squares are visited many times
  -- (especially those close to center).
  bresenhamsLineShooter mf x_extent y_extent =
    void $ flip execStateT (starting_point, 0, 0) $
      for_ [-y_extent..y_extent] $ \by -> when (by /= 0) $ do
        bresenhamLine x_extent by $ \x y -> do
          (previous_pos, previous_x, previous_y) <- get
          new_pos <- lift $
                     if | previous_x == x-1 && previous_y == y
                         -> (mf^.rightD) previous_pos
                        | previous_x == x-1 && previous_y == y-1
                         -> (mf^.downrightD) previous_pos
                        | previous_x == x && previous_y == y-1
                         -> (mf^.downD) previous_pos
                        | previous_x == x && previous_y == y+1
                         -> (mf^.upD) previous_pos
                        | previous_x == x-1 && previous_y == y+1
                         -> (mf^.uprightD) previous_pos
                        | previous_x == x && previous_y == y
                         -> return $ Just previous_pos
                        | otherwise -> error $ "computeLOS: I hit a place of code that should be dead code. Eep. Eek. " ++ show (x, y, previous_x, previous_y)

          case new_pos of
            Nothing -> return False
            Just really_new_pos -> do
              lift $ i_see really_new_pos

              put (really_new_pos, x, y)
              return $ see_through really_new_pos

        put (starting_point, 0, 0)
  straight pos move_function steps | steps > x_extent = return ()
                                   | otherwise = do
    next_pos <- move_function pos
    case next_pos of
      Nothing -> return ()
      Just new_pos -> do
        i_see new_pos
        let can_i_see_through = see_through new_pos
        when can_i_see_through $ straight new_pos move_function (steps+1)
{-# INLINE computeFieldOfView #-}

-- | Shoots a bresenham's line from origin to some place. Calls a function for
-- each spot it visits and stops if that function returns false.
bresenhamLine :: Monad m => Int -> Int -> (Int -> Int -> m Bool) -> m ()
bresenhamLine x1 y1 fun = do
  result <- fun 0 0
  when result $ loop 1 0 (2*y1 - x1)
 where
  loop x _ _ | x > x1 = return ()
  loop x y d =
    if d > 0
      then do result <- fun x (y+1)
              when result $ loop (x+1) (y+1) (d+2*y1-2*x1)
      else do result <- fun x y
              when result $ loop (x+1) y (d+2*y1)
-- Inline for Great Performance
{-# INLINE bresenhamLine #-}

