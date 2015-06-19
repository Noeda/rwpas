{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.AIControlledActor.BeastFrog
  ( BeastFrogState() )
  where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Data.Data
import Data.Foldable
import Data.SafeCopy
import GHC.Generics
import RWPAS.AIControlledActor.AIControlMonad
import RWPAS.AIControlledActor.Types
import RWPAS.Direction
import RWPAS.Level
import RWPAS.Turn
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
  , _spikesOut :: !Bool }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''BeastFrogState
deriveSafeCopy 0 'base ''BeastFrogState

instance IsAI BeastFrogState where
  initialState rng = do
    initial_stamina <- uniformR (3, 6 :: Int) rng
    return BeastFrogState { _staminaCounter = fromIntegral initial_stamina
                          , _spikesOut = False }

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
              then aiState.spikesOut .= True
              else hop

  spikes <- use $ aiState.spikesOut
  when spikes emitSpikes
 where
  emitSpikes =
    for_ directions8 $ \dir ->
      emitDecoration dir (Spikes dir)

  hop = do
    steps <- rollUniformR (2, 5)
    replicateM_ steps $ do
      dist <- distanceToPlayer
      d <- if dist < 15
        then getDirectionTowardsPlayer
        else rollUniform
      move d <|> return ()

