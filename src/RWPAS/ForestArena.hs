{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.ForestArena
  ( newForestArena )
  where

import Control.Lens hiding ( Level, inside )
import Control.Monad.Primitive
import Data.Data
import Data.Foldable
import Data.Set ( Set )
import qualified Data.Set as S
import GHC.Generics
import Linear.V2
import RWPAS.Actor
import RWPAS.Control
import RWPAS.Control.BeastFrog
import RWPAS.Level
import RWPAS.Rectangle
import RWPAS.World ( RunningID )
import System.Random.MWC

newForestArena :: forall m. PrimMonad m
               => Gen (PrimState m)
               -> RunningID
               -> m (Level, RunningID)
newForestArena rng rid = do
  houses <- placeHouse 100 S.empty rng
  lvl <- generateLevelM "Alma's Village" 500 500 $ \x y -> do
      -- distance from center
    let dist2 = sqrt $ fromIntegral $ (250-x)*(250-x) + (250-y)*(250-y) :: Double
      -- probability that this place should be empty
      -- (the 10 circle is guaranteed to be free)
      --
      -- bias it so that probability increases slowly at start but then jumps
        probability_raw = 1 - min 1 (max 0 ((dist2 - 20) / 210))
        probability = probability_raw**2

    let house_coords = V2 (x-250) (y-250)

    case find (inside house_coords . getHouseRectangle) houses of
      Just house ->
        if onEdge house_coords (getHouseRectangle house) && house_coords /= getDoorPosition house
          then return Planks
          else return PlanksFloor

      _ -> do which_variant <- uniform rng :: m Double
              p <- uniform rng
              if p > probability
                then return $ if which_variant > 0.3 then Tree1 else Tree2
                else return $ if which_variant > 0.2 then Grass else Dirt

  placeFrogs 500 rng rid lvl


type Tries = Int

data House = House Rectangle (V2 Int)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

getDoorPosition :: House -> V2 Int
getDoorPosition (House _ door) = door

getHouseRectangle :: House -> Rectangle
getHouseRectangle (House rect _) = rect

placeFrogs :: forall m. PrimMonad m
           => Int
           -> Gen (PrimState m)
           -> RunningID
           -> Level
           -> m (Level, RunningID)
placeFrogs 0 _ rid level = return (level, rid)
placeFrogs n rng rid level = do
  let V2 w h = levelSize level
  x <- uniformR (0, w-1) rng
  y <- uniformR (0, h-1) rng
  let tf = terrainFeature (V2 x y) level
  if impassable tf
    then placeFrogs n rng rid level
    else case actorByCoordinates (V2 x y) level of
           Nothing -> do
             frog_ai <- initialState rng :: m BeastFrogState
             let new_level = bestowAI rid (AI frog_ai) $
                             insertActor rid (sentinelActor "Type-A Frog" &
                               (position .~ V2 x y) .
                               (actorHitPoints .~
                                (Just $ emptyHitPoints &
                                  (maxHp .~ 20) .
                                  (hp .~ 20))) .
                               (appearance .~ BeastFrog)) level
             placeFrogs (n-1) rng (rid+1) new_level
           Just _ -> placeFrogs n rng rid level

placeHouse :: PrimMonad m
           => Tries
           -> Set House
           -> Gen (PrimState m)
           -> m (Set House)
placeHouse 0 set _ = return set
placeHouse t set rng = do
  w <- uniformR (5, 10) rng
  h <- uniformR (5, 10) rng
  x <- uniformR (-60, 50) rng
  y <- uniformR (-60, 50) rng

  which_side_door <- uniformR (0 :: Int, 3) rng
  door_place <- if which_side_door <= 1
                  then do dx <- uniformR (x+1, x+w-2) rng
                          let dy = if which_side_door == 0
                                     then y
                                     else y+h-1
                          return (V2 dx dy)
                  else do dy <- uniformR (y+1, y+h-2) rng
                          let dx = if which_side_door == 2
                                     then x
                                     else x+w-1
                          return (V2 dx dy)

  let rect = ltrb x y (x+w-1) (y+h-1)

  if any (overlaps (enlarge1 $ enlarge1 rect) . getHouseRectangle) set
    then placeHouse (t-1) set rng
    else placeHouse t (S.insert (House rect door_place) set) rng


