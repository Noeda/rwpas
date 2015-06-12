-- | This module implements whatever is needed to play RWPAS locally in a
-- terminal. Based on the \'ansi-terminal\' Haskell package.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module RWPAS.AnsiTerminalOutput
  (
  runAnsiTerminalRWPAS
  )
  where

import Control.Exception
import Control.Lens hiding ( Level )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Foldable
import Data.Data
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Linear.V2
import RWPAS.Actor
import RWPAS.Control
import RWPAS.Direction
import RWPAS.Level
import System.Console.ANSI
import System.IO
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.User

foreign import ccall get_window_size :: Ptr CInt -> Ptr CInt -> IO ()

data Square = Square
  { _character :: !Char
  , _foregroundColor :: !Color
  , _backgroundColor :: !Color }
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )
makeLenses ''Square

getWindowSize :: IO Size
getWindowSize =
  alloca $ \w_ptr ->
  alloca $ \h_ptr -> do
    get_window_size w_ptr h_ptr
    V2 <$> (fromIntegral <$> peek w_ptr) <*>
           (fromIntegral <$> peek h_ptr)

type Offset = V2 Int

runAnsiTerminalRWPAS :: IO ()
runAnsiTerminalRWPAS = do
  hSetBuffering stdin NoBuffering
  ta <- getTerminalAttributes stdInput
  let new_ta = withoutMode ta EnableEcho

  flip finally (setTerminalAttributes stdInput ta WhenFlushed *> showCursor) $ do
    setTerminalAttributes stdInput new_ta WhenFlushed

    splashScreen

    username <- getEffectiveUserName
    putStrLn $ "Welcome, " ++ username ++ "."
    putStrLn "Press space to start or q if you don't want to play after all."

    waitUntilStart
 where
  waitUntilStart = do
    c <- getChar
    if | c == ' ' -> startGame
       | c == 'q' -> putStrLn "Goodbye."
       | otherwise -> waitUntilStart

splashScreen :: IO ()
splashScreen = do
  setCursorPosition 0 0
  hideCursor
  clearScreen
  putStrLn "Roguelike With Portals And Stuff"
  putStrLn "(c) 2015 Mikko Juola"
  putStrLn ""

startGame :: IO ()
startGame = do
  let initial_world = singletonWorld (roomLevel (V2 20 20))

  void $ flip execStateT initial_world $ forever $ do
    world <- get
    V2 tw th <- liftIO getWindowSize

    let (level, actor) = currentActorLevelAndCoordinates world
        actor_pos = actor^.position
        offset = V2 ((actor_pos^._x) - (tw `div` 2))
                    ((actor_pos^._y) - (th `div` 2))

    liftIO $ writeLevel offset level

    cmd <- liftIO getNextCommand
    modify $ performCommand cmd
 where
  getNextCommand = do
    maybe_cmd <- charToCommand <$> getChar
    case maybe_cmd of
      Nothing -> getNextCommand
      Just cmd -> return cmd


charToCommand :: Char -> Maybe Command
charToCommand 'h' = Just (Move DLeft)
charToCommand 'k' = Just (Move DUp)
charToCommand 'j' = Just (Move DDown)
charToCommand 'l' = Just (Move DRight)
charToCommand _ = Nothing

appearanceToCell :: ActorAppearance -> Square
appearanceToCell PlayerCharacter = Square '@' White Black

-- | Mapping from level features to characters to show on screen.
featureToCell :: TerrainFeature -> Square
featureToCell Floor = Square '.' White Black
featureToCell Wall = Square '#' White Black
featureToCell Rock = Square ' ' White Black

-- | Writes a level on the screen, at given offset.
--
-- This overwrites the whole screen.
writeLevel :: Offset
           -> Level
           -> IO ()
writeLevel offset level = do
  -- TODO: don't so wastefully write to terminal, we could get by with much
  -- less.
  setSGR [Reset]
  V2 tw th <- getWindowSize

  let set_cell_attributes cell =
        setSGR [SetColor Foreground Dull (cell^.foregroundColor)
               ,SetColor Background Dull (cell^.backgroundColor)]

  for_ [0..th-1] $ \y -> do
    setCursorPosition y 0
    for_ [0..tw-1] $ \x -> do
      let feature = level^.terrainFeature (V2 x y + offset)
          cell = featureToCell feature

      set_cell_attributes cell
      putChar (cell^.character)

  for_ (level^.actors) $ \actor -> do
    let actor_pos@(V2 ax ay) = actor^.position - offset
        cell = appearanceToCell $ actor^.appearance

    unless (ax < 0 || ay < 0 || ax >= tw || ay >= th) $ do
      setCursorPosition (actor_pos^._y) (actor_pos^._x)

      set_cell_attributes cell
      putChar (cell^.character)

  hFlush stdout

  setCursorPosition 0 0

