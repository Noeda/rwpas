{-# LANGUAGE RankNTypes #-}

module RWPAS.Control where
  import Control.Lens hiding ( Level )
  import {-# SOURCE #-} RWPAS.Level
  data World
  levelById :: LevelID -> Lens' World (Maybe Level)

