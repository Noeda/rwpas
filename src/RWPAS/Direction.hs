{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.Direction
  ( Direction4(..)
  , Direction8(..)
  , direction4ToDelta
  , direction8ToDelta
  , direction4To8
  , directions8 )
  where

import Data.Data
import GHC.Generics
import Linear.V2
import System.Random.MWC

-- | The four directions.
data Direction4
  = DUp
  | DLeft
  | DRight
  | DDown
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | The eight directions.
data Direction8
  = D8Up
  | D8Left
  | D8Right
  | D8Down
  | D8UpLeft
  | D8UpRight
  | D8DownLeft
  | D8DownRight
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | `uniformR` is not very sensible but it is implemented.
instance Variate Direction8 where
  uniform rng = do
    x <- uniformR (0, 7) rng
    return $ toEnum x

  uniformR (d1, d2) rng = do
    x <- uniformR (i1, i2) rng
    return $ toEnum x
   where
    i1 = fromEnum d1
    i2 = fromEnum d2

-- | `uniformR` is not very sensible but it is implemented.
instance Variate Direction4 where
  uniform rng = do
    x <- uniformR (0, 3) rng
    return $ toEnum x

  uniformR (d1, d2) rng = do
    x <- uniformR (i1, i2) rng
    return $ toEnum x
   where
    i1 = fromEnum d1
    i2 = fromEnum d2

directions8 :: [Direction8]
directions8 = [D8Up, D8Left, D8Right, D8Down
              ,D8UpLeft, D8UpRight, D8DownLeft, D8DownRight]

direction4ToDelta :: Direction4 -> V2 Int
direction4ToDelta DUp = V2 0 (-1)
direction4ToDelta DLeft = V2 (-1) 0
direction4ToDelta DRight = V2 1 0
direction4ToDelta DDown = V2 0 1

direction8ToDelta :: Direction8 -> V2 Int
direction8ToDelta D8Up = V2 0 (-1)
direction8ToDelta D8Left = V2 (-1) 0
direction8ToDelta D8Right = V2 1 0
direction8ToDelta D8Down = V2 0 1
direction8ToDelta D8UpLeft = V2 (-1) (-1)
direction8ToDelta D8UpRight = V2 1 (-1)
direction8ToDelta D8DownLeft = V2 (-1) 1
direction8ToDelta D8DownRight = V2 1 1

direction4To8 :: Direction4 -> Direction8
direction4To8 DUp = D8Up
direction4To8 DDown = D8Down
direction4To8 DLeft = D8Left
direction4To8 DRight = D8Right

