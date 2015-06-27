{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module RWPAS.Role.Psychic
  ( AuraType(..)
  , activate )
  where

import Control.Lens
import Control.Monad.State.Strict
import Data.Data
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.IntSet as IS
import Data.Set ( Set )
import Data.Text ( Text )
import Linear.Metric
import Linear.V2
import GHC.Generics
import RWPAS.Actor
import RWPAS.BresenhamLine
import RWPAS.Control.ControlMonad
import RWPAS.Direction
import RWPAS.Level.Type
import RWPAS.World.Type

data Psychic = Psychic
  { _learnedAuras :: Set AuraType }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data AuraType
  = RepulsionForce
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
makeLenses ''Psychic

auraName :: AuraType -> Text
auraName RepulsionForce = "repulsion force"

auraHelp :: AuraType -> Text
auraHelp RepulsionForce
  = "Emits a field of force that pushes back foes, sometimes violently."

activate :: ControlActorMonad s m
         => AuraType
         -> WorldCoordinates
         -> m ()
activate RepulsionForce coords = repulsionActivate coords

repulsionActivate :: ControlActorMonad s m
                  => WorldCoordinates
                  -> m ()
repulsionActivate coords = do
  immune_aid <- myActorID

  -- keep a state of actors that have already been repulsed/are immune
  void $ flip evalStateT (IS.singleton immune_aid) $ do

    -- shoot bresenham to the edge of the repulsion field

    -- top and bottom
    for_ [-10..10] $ \x -> do
      shootRepulsionLine (V2 x (-10))
      shootRepulsionLine (V2 x 10)

    -- left and right (-9 to 9 because corners were already taken above)
    for_ [-9..9] $ \y -> do
      shootRepulsionLine (V2 (-10) y)
      shootRepulsionLine (V2 10 y)

  r <- rollUniformR (0, 4 :: Int)
  emitMessage $ case r of
    0 -> "KABOOOM!!"
    1 -> "BATAANNNGG!!!"
    2 -> "DONNNGG!"
    3 -> "KATANNGGGG!!"
    _ -> "BAZANGGGG!"
 where
  shootRepulsionLine target_coords =
    void $ flip execStateT coords $
      bresenhamLineDirectionP target_coords $ \dir relative_coords -> do
        my_pos <- get
        new_pos <- lift $ lift $ moveCoords dir my_pos
        put new_pos

        f <- lift $ lift $ use (world.terrainAt new_pos)

        case f of
          Just t | impassable t -> return False
          _ -> do excluded <- lift get

                  (lift $ lift $ use $ world.actorAt new_pos) >>= \case
                    Just (victim_id, victim) | IS.notMember victim_id excluded -> do
                      let power = 20.0 / (norm $ fmap fromIntegral relative_coords)

                      lift $ modify $ IS.insert victim_id
                      lift $ lift $ hurlVictim new_pos momentum power
                      return True
                    _ -> return True
   where
    momentum :: V2 Double
    momentum = signorm $ fmap fromIntegral target_coords

hurlVictim :: ControlMonad s m
           => WorldCoordinates
           -> V2 Double
           -> Double
           -> m ()
hurlVictim coords direction@(V2 dx'' dy'') original_power =
  use (world.actorAt coords) >>= \case
    Nothing -> return ()
    Just ac -> loop ac coords coords original_power
 where
  dx' = abs dx''
  dy' = abs dy''
  dx = dx' / (dx' + dy')
  dy = dy' / (dx' + dy')

  move_x_dir = if dx'' >= 0 then D8Right else D8Left
  move_y_dir = if dy'' >= 0 then D8Down else D8Up

  loop ac@(_, actor) last_coords current_coords power | power > 0 = do
    f <- use (world.terrainAt current_coords)
    a <- use (world.actorAt current_coords)
    if (impassable <$> f) /= Just False || (isJust a && last_coords /= current_coords)
      then do case a^?_Just._2.actorName of
                Just name ->
                  emitMessage $ actor^.actorName <> " collides with " <> name <> "!"
                _ -> emitMessage $ actor^.actorName <> " smashes into an obstacle!"
              hurlTarget ac last_coords True
      else do rx <- rollUniformR (0, 1 :: Double)
              ry <- rollUniformR (0, 1 :: Double)
              coords1 <- if rx < dx then moveOnXAxis else return current_coords
              coords2 <- if ry < dy then moveOnYAxis else return coords1
              loop ac current_coords coords2 (power-1)
   where
    moveOnXAxis = moveCoords move_x_dir current_coords
    moveOnYAxis = moveCoords move_y_dir current_coords

  loop ac final_coords _ _ = hurlTarget ac final_coords False

  hurlTarget ac@(actor_id, actor) final_coords hurt_it = do
    world.actorAt coords .= Nothing
    world.actorAt final_coords .= Just (if hurt_it
                                          then (actor_id, hurt (ceiling original_power) actor)
                                          else ac)

