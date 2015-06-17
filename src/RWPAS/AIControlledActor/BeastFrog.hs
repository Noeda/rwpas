{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module RWPAS.AIControlledActor.BeastFrog
  ( BeastFrogState() )
  where

import Control.Lens
import Data.Data
import Data.SafeCopy
import GHC.Generics
import RWPAS.AIControlledActor.Types
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
  { _staminaCounter :: !Turns }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''BeastFrogState
deriveSafeCopy 0 'base ''BeastFrogState

instance IsAI BeastFrogState where
  initialState rng = do
    initial_stamina <- uniformR (5, 10 :: Int) rng
    return BeastFrogState { _staminaCounter = fromIntegral initial_stamina }

  transitionFunction = beastFrogTransition

beastFrogTransition :: AITransition a
beastFrogTransition = runAIControlMonad $
  return ()


