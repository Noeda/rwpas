{-# LANGUAGE RankNTypes #-}

module RWPAS.World where
  import Control.Lens hiding ( Level )
  import RWPAS.Actor
  import {-# SOURCE #-} RWPAS.Level
  data World
  currentLevelAndActor :: World -> (Level, LevelID, Actor, ActorID)
  levelById :: LevelID -> Lens' World (Maybe Level)

