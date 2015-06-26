{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module RWPAS.Control.BeastFrog
  ( BeastFrogState() )
  where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Data.Data
import Data.Foldable
import Data.Maybe
import Data.SafeCopy
import Data.Set ( Set )
import qualified Data.Set as S
import GHC.Generics
import RWPAS.Actor
import RWPAS.Control.ControlMonad
import RWPAS.Control.Types
import RWPAS.Direction
import RWPAS.Level
import RWPAS.Turn
import RWPAS.World
import System.Random.MWC

-- Beast frogs:
--
-- They hop around and stay still a lot.
--
-- When next to player (and if hostile), spikes can come out of their skin,
-- piercing and pushing back the player. The spikes also hurt any monster next
-- to them.

data BeastFrogState = BeastFrogState
  { _staminaCounter :: !Turns
  , _spikesOut :: !Bool
  , _bloodySpikes :: !(Set Direction8) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''BeastFrogState
deriveSafeCopy 0 'base ''BeastFrogState

instance IsAI BeastFrogState where
  initialState rng = do
    initial_stamina <- uniformR (3, 6 :: Int) rng
    return BeastFrogState { _staminaCounter = fromIntegral initial_stamina
                          , _spikesOut = False
                          , _bloodySpikes = S.empty }

  transitionFunction = beastFrogTransition

  aiName _ = "BeastFrog"

beastFrogTransition :: PrimMonad m => AITransition m BeastFrogState
beastFrogTransition = runAIControlMonad $ do
  stamina <- use $ aiState.staminaCounter
  if stamina > 0
    then do aiState.staminaCounter -= 1
            return ()
    else do r <- rollUniformR (3, 6 :: Int)
            aiState.spikesOut .= False
            aiState.staminaCounter += fromIntegral r

            dist <- distanceToPlayer
            if dist <= 1
              then do aiState.spikesOut .= True
                      aiState.bloodySpikes .= S.empty
                      impaleNeighbours
              else hop

  withNChance 30 emitNoises

  spikes <- use $ aiState.spikesOut
  when spikes emitSpikes
 where
  emitSpikes = do
    bloodied_ones <- use (aiState.bloodySpikes)
    for_ directions8 $ \dir ->
      emitDecoration dir (if S.member dir bloodied_ones
                            then BloodySpikes dir
                            else Spikes dir)

  emitNoises = do
    dist <- distanceToPlayer
    if | dist < 5     -> emitMessage "RIBBIT!!"
       | dist < 10    -> emitMessage "Ribbit!"
       | dist < 30    -> emitMessage "ribbit"
       | otherwise -> return ()

  impaleNeighbours = do
    base_coords <- myCoordinates
    aid <- myActorID
    w <- use world
    let (_, _, _, player_id) = currentLevelAndActor w

    for_ directions8 $ \dir ->
      (do impaled_coords <- moveCoords dir base_coords
          world.actorAt impaled_coords._Just._2 %= hurt 5
          -- Push back the actor...if we can
          -- Only push back one step and don't push more than once
          -- Otherwise strategically placed portals could cause an infinite
          -- loop, monsters pushing themselves.
          ac <- use (world.actorAt impaled_coords)
          when (isJust ac) $ do
            aiState.bloodySpikes %= S.insert dir
            let Just (impaled_aid, _) = ac
            pushed_back_coords <- moveCoords dir impaled_coords
            pushed_back_actor <- use (world.actorAt pushed_back_coords)
            pushed_back_feature <- use (world.terrainAt pushed_back_coords)

            when (impaled_aid == player_id) $ emitMessage "You are impaled!"

            case (pushed_back_actor, pushed_back_feature) of
                -- Push back if there's free space behind and we are not
                -- impaling ourselves.
                (Nothing, Just f) | not (impassable f) && impaled_aid /= aid -> do
                  world.actorAt impaled_coords .= Nothing
                  world.actorAt pushed_back_coords .= ac
                _ -> return ()
       ) <|> return ()

  hop = do
    steps <- rollUniformR (2, 5)
    replicateM_ steps $ do
      dist <- distanceToPlayer
      d <- if dist < 15
        then getDirectionTowardsPlayer
        else rollUniform
      move d <|> return ()

