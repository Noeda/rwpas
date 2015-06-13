-- | Line of sight, an abstract implementation, implemented so that it can be
-- easily used in portal implementaiton.
--

{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Data
import GHC.Generics
import Linear.V2

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
makeLenses ''ByDirection

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

flipY :: ByDirection a -> ByDirection a
flipY (ByDirection{..}) = ByDirection
  { _leftD = _leftD
  , _rightD = _rightD
  , _leftupD = _leftdownD
  , _uprightD = _downrightD
  , _leftdownD = _leftupD
  , _downrightD = _uprightD
  , _upD = _downD
  , _downD = _upD }

-- Rotate clockwise
{-rotate90 :: ByDirection a -> ByDirection a
rotate90 (ByDirection{..}) = ByDirection
  { _leftD = _downD
  , _upD = _leftD
  , _rightD = _upD
  , _downD = _rightD

  , _leftupD = _leftdownD
  , _uprightD = _leftupD
  , _downrightD = _uprightD
  , _leftdownD = _downrightD }-}

numSlopes :: Int
numSlopes = 50

-- | Computes the field of view in two dimensions.
--
-- Very expensive with the current algorithm. The benchmark says 10ms to
-- compute the view for 100x100 completely open area (which is the most
-- pathological case).
computeFieldOfView :: forall a m. Monad m
           => (a -> m ()) -- ^ I can see this square. May be called more than once.
           -> (a -> m Bool) -- ^ Can you see through this square?
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

  beamlosShooter move_functions x_extent y_extent
  beamlosShooter (flipY move_functions) x_extent y_extent
  beamlosShooter (flipX move_functions) x_extent y_extent
  beamlosShooter (flipY $ flipX move_functions) x_extent y_extent
 where
  -- http://www.roguebasin.com/index.php?title=Isaac_s_fast_beamcasting_LOS
  --
  -- Suits the portal mechanism well because the algorithm is based on shooting
  -- beams.
  beamlosShooter :: ByDirection (a -> m (Maybe a)) -> Int -> Int -> m ()
  beamlosShooter mf x_extent y_extent =
    loop 1
   where
    loop slope | slope >= numSlopes = return ()
    loop slope =
      loop2 slope 0 (numSlopes-1) (Just starting_point) (Just starting_point) (V2 0 0) (V2 0 0) 1
     where
      loop2 :: Int -> Int -> Int -> Maybe a -> Maybe a -> V2 Int -> V2 Int -> Int -> m ()
      loop2 _ mini maxi _ _ _ _ _ | mini > maxi = loop (slope+1)
      loop2 v mini maxi previous_mini previous_maxi previous_mini_coords previous_maxi_coords u = do
        let y   = v `div` numSlopes
            x   = u - y
            cor = numSlopes - (v `mod` numSlopes)

            new_mini_coords = V2 x y
            new_maxi_coords = V2 (x-1) (y+1)

        current_mini <- move previous_mini previous_mini_coords new_mini_coords
        current_maxi <- move previous_maxi previous_maxi_coords new_maxi_coords

        let within_bounds_mini = x <= x_extent && y <= y_extent
            within_bounds_maxi = (x-1) <= x_extent && (y+1) <= y_extent

        new_mini <- if mini < cor
          then do do_i_see_it <- case current_mini of
                       Just amini -> do i_see amini
                                        see_through amini
                       Nothing -> return False
                  return $ if do_i_see_it && within_bounds_mini
                    then mini
                    else cor
          else return mini

        new_maxi <- if maxi > cor
          then do do_i_see_it <- case current_maxi of
                       Just amaxi -> do i_see amaxi
                                        see_through amaxi
                       Nothing -> return False
                  return $ if do_i_see_it && within_bounds_maxi
                    then maxi
                    else cor
          else return maxi

        loop2 (v+slope)
              new_mini
              new_maxi
              current_mini
              current_maxi
              new_mini_coords
              new_maxi_coords
              (u+1)

      move Nothing _ _ = return Nothing
      move (Just item) (V2 ox oy) (V2 nx ny) | ox == nx && oy == ny = return $ Just item
      move (Just item) (V2 ox oy) (V2 nx ny) =
       if | nx == ox
           -> (if | ny == oy+1 -> (mf^.downD) item
                  | ny == oy-1 -> (mf^.upD) item
                  | otherwise -> error "beamlosShooter: impossible")
          | ny == oy
           -> (if | nx == ox+1 -> (mf^.rightD) item
                  | nx == ox-1 -> (mf^.leftD) item
                  | otherwise -> error "beamLosShooter: impossible")
          | otherwise
           -> (if | nx == ox+1 && ny == oy+1 -> (mf^.downrightD) item
                  | nx == ox+1 && ny == oy-1 -> (mf^.uprightD) item
                  | nx == ox-1 && ny == oy+1 -> (mf^.leftdownD) item
                  | nx == ox-1 && ny == oy-1 -> (mf^.leftupD) item
                  | otherwise -> error "beamLosShooter: impossible")

  straight pos move_function steps | steps > x_extent = return ()
                                   | otherwise = do
    next_pos <- move_function pos
    case next_pos of
      Nothing -> return ()
      Just new_pos -> do
        i_see new_pos
        can_i_see_through <- see_through new_pos
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

