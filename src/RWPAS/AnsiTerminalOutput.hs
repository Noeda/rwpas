-- | This module implements whatever is needed to play RWPAS locally in a
-- terminal. Based on the \'ansi-terminal\' Haskell package.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Linear.V2
import RWPAS.Actor
import RWPAS.Control
import RWPAS.Direction
import RWPAS.ForestArena
import RWPAS.Level
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.User
import System.Random.MWC

foreign import ccall get_window_size :: Ptr CInt -> Ptr CInt -> IO ()

data Square = Square
  { _character :: !Char
  , _foregroundColor :: !Color
  , _boldForeground :: !Bool
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

data Option
  = SetUsername String
  | ShowHelp
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

options :: [OptDescr Option]
options = [Option "u" ["username"] (ReqArg SetUsername "username") "set username"
          ,Option "h?" ["help"] (NoArg ShowHelp) "show help"]

showHelp :: IO ()
showHelp = putStrLn $ usageInfo "rwpas" options

handleArguments :: IO String
handleArguments = do
  args <- getArgs
  username <- getEffectiveUserName
  case getOpt Permute options args of
    (opts, [], []) | ShowHelp `elem` opts
      -> showHelp *> exitSuccess
    (opts, [], []) -> return $ handleOpts opts username
    (_, non_opts, errs) ->
      error $ "Invalid arguments: " ++ show (non_opts, errs)
 where
  handleOpts [] username = username
  handleOpts (ShowHelp:rest) username = handleOpts rest username
  handleOpts (SetUsername username:rest) _ =
    handleOpts rest username

runAnsiTerminalRWPAS :: IO ()
runAnsiTerminalRWPAS = do
  username <- handleArguments
  when (username == "") $ error "Username cannot be empty."

  hSetBuffering stdin NoBuffering
  ta <- getTerminalAttributes stdInput
  let new_ta = withoutMode ta EnableEcho

  flip finally (setTerminalAttributes stdInput ta WhenFlushed *> showCursor) $ do
    setTerminalAttributes stdInput new_ta WhenFlushed

    splashScreen

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
  rng <- createSystemRandom
  initial_world <- singletonWorld <$> newForestArena rng

  setSGR [Reset]
  clearScreen

  V2 tw th <- liftIO getWindowSize
  let initial_cache = newCache tw th

  gameLoop initial_world initial_cache tw th
 where
  newCache w h = M.fromList [ (V2 x y, Square ' ' White False Black) |
                              x <- [0..w-1]
                            , y <- [0..h-1] ]

  gameLoop world cache last_tw last_th = do
    V2 tw th <- getWindowSize

    actual_cache <- if tw /= last_tw || th /= last_th
                      then clearScreen *> return (newCache tw th)
                      else return cache

    new_cache <- writeLevel world actual_cache

    cmd <- getNextCommand
    gameLoop (performCommand cmd world) new_cache tw th

  getNextCommand = do
    maybe_cmd <- charToCommand <$> getChar
    case maybe_cmd of
      Nothing -> getNextCommand
      Just cmd -> return cmd


charToCommand :: Char -> Maybe Command
charToCommand 'h' = Just (Move D8Left)
charToCommand 'k' = Just (Move D8Up)
charToCommand 'j' = Just (Move D8Down)
charToCommand 'l' = Just (Move D8Right)
charToCommand 'y' = Just (Move D8UpLeft)
charToCommand 'u' = Just (Move D8UpRight)
charToCommand 'b' = Just (Move D8DownLeft)
charToCommand 'n' = Just (Move D8DownRight)
charToCommand _ = Nothing

appearanceToCell :: ActorAppearance -> Square
appearanceToCell PlayerCharacter = Square '@' White True Black

-- | Mapping from level features to characters to show on screen.
featureToCell :: TerrainFeature -> Square
featureToCell Floor = Square '.' White False Black
featureToCell Wall = Square '#' White False Black
featureToCell Rock = Square ' ' White False Black
featureToCell Tree1 = Square 'T' Green True Black
featureToCell Tree2 = Square 'T' Green False Black
featureToCell Grass = Square '.' Green True Black
featureToCell Planks = Square '#' Yellow True Black
featureToCell PlanksFloor = Square '.' Yellow True Black
featureToCell Dirt = Square '.' Yellow False Black

type ScreenCache = Map (V2 Int) Square

-- | Writes a level on the screen, at given offset.
--
-- This overwrites the whole screen.
writeLevel :: World
           -> ScreenCache
           -> IO ScreenCache
writeLevel world cache = do
  -- TODO: don't so wastefully write to terminal, we could get by with much
  -- less.
  setSGR [Reset]
  V2 tw th <- getWindowSize

  let set_cell_attributes cell =
        setSGR [SetColor Foreground (if cell^.boldForeground
                                       then Vivid
                                       else Dull) (cell^.foregroundColor)
               ,SetColor Background Dull (cell^.backgroundColor)]

      (new_cache, changed_spots) = flip execState (cache, S.empty) $ do
        let modifier cell tcoords = modify $ \(old_cache, spots) ->
                                    case M.lookup tcoords old_cache of
                                      Just old_cell | old_cell /= cell ->
                                        (M.insert tcoords cell old_cache
                                        ,S.insert tcoords spots)
                                      _ -> (old_cache, spots)

        for_ [0..th-1] $ \y ->
          for_ [0..tw-1] $ \x ->
            let tcoords = V2 x y
             in modifier (Square ' ' White False Black) tcoords

        let (w, h, access) = getCurrentFieldOfView world
            (lvl, _, actor, actor_id) = currentActorLevelAndCoordinates world

        for_ [0..h-1] $ \y ->
          for_ [0..w-1] $ \x -> do
            let ox = x + tw `div` 2 - (w `div` 2)
                oy = y + th `div` 2 - (h `div` 2)
                op = V2 ox oy
                lp = V2 (actor^.position._x + x - (w `div` 2))
                        (actor^.position._y + y - (h `div` 2))

            case access x y of
              (Just ap, _) -> modifier (appearanceToCell ap) op
              (_, Just t)  -> modifier (featureToCell t) op
              _ -> case getMemoryAt actor_id lvl lp of
                Nothing -> return ()
                Just v  ->
                  modifier (let Square ch _ _ _ = featureToCell v
                             in Square ch Black True Black) op

  for_ changed_spots $ \coords -> do
    let cell = fromMaybe (Square ' ' White False Black) $ M.lookup coords new_cache
        (V2 x y) = coords
    when (x >= 0 && y >= 0 && x < tw && y < th) $ do
      setCursorPosition y x
      set_cell_attributes cell
      putChar (cell^.character)

  hFlush stdout

  setCursorPosition 0 0
  return new_cache

