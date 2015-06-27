module RWPAS.RNG
  ( HasRNG(..) )
  where

import System.Random.MWC

class HasRNG m where
  rollUniform :: Variate v => m v
  rollUniformR :: Variate v => (v, v) -> m v

