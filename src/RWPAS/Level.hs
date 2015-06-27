module RWPAS.Level
  ( Level()
  , cycleLevel
  , bestowAI
  , module RWPAS.Level.Type )
  where

import Control.Lens hiding ( Level, levels )
import Control.Monad
import Control.Monad.State.Strict
import RWPAS.Actor
import RWPAS.Control.Types
import RWPAS.Level.Type
import RWPAS.World.Type
import Control.Monad.Primitive
import System.Random.MWC

-- | Simulates all actors on the level for one cycle.
cycleLevel :: PrimMonad m => LevelID -> World -> Gen (PrimState m) -> m World
cycleLevel level_id world rng =
  case world^.levelById level_id of
    Nothing -> return world
    Just lvl ->
      let new_world = world & levelById level_id .~ Just (removeDecorations lvl)
       in flip execStateT new_world $ do
            -- Step all the actor AIs
            iforOf_ eachActor lvl $ \aid ac -> do
              let actor_ai = ac^.ai
              w <- get
              case w^.levelById level_id of
                Nothing -> return ()
                Just lvl -> case lvl^.actorById aid of
                  Nothing -> return ()
                  Just _ -> do
                    new_world <- stepAI actor_ai rng w aid level_id
                    put new_world

            -- Remove actors that have had their HP drop zero
            w <- get
            let (_, _, _, player_id) = currentLevelAndActor w
            case w^.levelById level_id of
              Nothing -> return ()
              Just new_level ->
                  iforOf_ eachActor new_level $ \aid _ ->
                  -- Don't remove the player actor. Player actors are
                  -- immune to being removed by hp loss (this case is
                  -- handled specially)
                  when (aid /= player_id) $ do
                    w <- get
                    case w^.levelById level_id of
                      Just lvl ->
                        case lvl^.actorById aid of
                          Just ac -> case ac^?actorHitPoints._Just.hp of
                            Just x | x <= 0 -> do
                              old_world <- get
                              new_world <- stepDeadAI (ac^.ai) rng old_world aid level_id
                              put new_world
                            _ -> return ()
                          _ -> return ()
                      _ -> return ()

-- | Gives an artificial intelligence to an actor.
--
-- Replaces any old AI and its state.
bestowAI :: ActorID -> AI -> Level -> Level
bestowAI aid actor_ai level =
  case level^.actorById aid of
    Nothing -> level
    Just _ -> level & actorById aid._Just.ai .~ actor_ai

