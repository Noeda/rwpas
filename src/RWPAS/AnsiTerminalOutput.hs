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

import Control.Concurrent
import Control.Exception
import Control.Lens hiding ( Level )
import Data.Foldable
import Data.Data
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Linear.V2
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
  writeLevel (V2 (-5) (-5)) (roomLevel (V2 10 10))

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
  print (tw, th)
  for_ [0..th-1] $ \y -> do
    setCursorPosition y 0
    for_ [0..tw-1] $ \x -> do
      let feature = level^.terrainFeature (V2 x y + offset)
          cell = featureToCell feature

      setSGR [SetColor Foreground Dull (cell^.foregroundColor)
             ,SetColor Background Dull (cell^.backgroundColor)]
      putChar (cell^.character)
  setCursorPosition 0 0
  threadDelay 10000000

