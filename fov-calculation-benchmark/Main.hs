-- | Benchmark for the field of view algorithm in the pathological case
-- (nothing obstructing anything)
--

{-# LANGUAGE BangPatterns #-}

module Main ( main ) where

import Control.Monad.Trans.State.Strict
import Criterion
import Criterion.Main
import Linear.V2
import RWPAS.Control
import RWPAS.Level

main :: IO ()
main =
  let initial_world = singletonWorld (roomLevel (V2 200 200))
      (level, _) = currentActorLevelAndCoordinates initial_world
   in level `seq` initial_world `seq`
      defaultMain [bench "fov benchmark" $
                   whnf (`execState` (0 :: Int)) $
                   levelFieldOfView (V2 100 100)
                                    level
                                    (\(!_) -> modify (+1))]

