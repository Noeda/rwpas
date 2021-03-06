-- | This module implements whatever is needed to play RWPAS locally in a
-- terminal. Based on the \'ansi-terminal\' Haskell package.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

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
import Codec.Compression.Zlib
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Data
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.SafeCopy
import Data.Serialize.Get
import Data.Serialize.Put
import qualified Data.Set as S
import qualified Data.Text as T
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Linear.V2
import RWPAS.Actor
import RWPAS.Direction
import RWPAS.ForestArena
import RWPAS.Item
import RWPAS.Level
import RWPAS.World
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
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

    maybe_world <- tryLoadSave
    case maybe_world of
      Left "" -> do
        putStrLn "Press space to start or q if you don't want to play after all."
        waitUntilStart username Nothing
      Left err -> do
        putStrLn $ "Loading a world failed: " <> err
        putStrLn "If you press start, I'll start a new game for you. Or you can press q to quit."
        waitUntilStart username Nothing
      Right world -> do
        putStrLn "Press space to continue old game or q if you don't want to play after all."
        waitUntilStart username (Just world)
 where
  waitUntilStart username w = do
    c <- getChar
    if | c == ' ' -> startGame username w
       | c == 'q' -> putStrLn "Goodbye."
       | otherwise -> waitUntilStart username w

tryLoadSave :: IO (Either String World)
tryLoadSave =
  tryIOError (decompress . BL.fromStrict <$> B.readFile "rwpas.save") >>= \case
    Left _ -> return (Left "")
    Right bs -> return $ case runGet safeGet (BL.toStrict bs) of
      Left err -> Left err
      Right w -> Right w

splashScreen :: IO ()
splashScreen = do
  setCursorPosition 0 0
  hideCursor
  clearScreen
  putStrLn "Roguelike With Portals And Stuff"
  putStrLn "(c) 2015 Mikko Juola"
  putStrLn ""

startGame :: String -> Maybe World -> IO ()
startGame username loaded_world = do
  rng <- createSystemRandom
  initial_world <- case loaded_world of
    Just w -> return w
    Nothing -> do
      (forest, rid) <- newForestArena rng 3
      let initial_world' = singletonWorld forest
      return $ initial_world' & runningID .~ rid

  setSGR [Reset]
  clearScreen

  V2 tw th <- liftIO getWindowSize
  let initial_cache = newCache tw th

  gameLoop initial_world initial_cache tw th rng
 where
  newCache w h = M.fromList [ (V2 x y, Square ' ' White False Black) |
                              x <- [0..w-1]
                            , y <- [0..h-1] ]

  gameLoop world cache last_tw last_th rng = do
    V2 tw th <- getWindowSize

    actual_cache <- if tw /= last_tw || th /= last_th
                      then clearScreen *> return (newCache tw th)
                      else return cache

    new_cache <- writeLevel world actual_cache username

    cmd <- getNextCommand
    case cmd of
      WorldCommand wcmd -> performCommand wcmd rng world >>= \case
        Nothing -> gameLoop world new_cache tw th rng
        Just w -> do
          new_world' <- cycleWorld rng w
          let new_world'' = computeFieldOfView new_world'
          gameLoop new_world'' new_cache tw th rng

      Save -> do
        let save_file = "rwpas.save"
            saved = runPut (safePut world)
            zipped = compress (BL.fromStrict saved)
        BL.writeFile save_file zipped

  getNextCommand = do
    maybe_cmd <- charToCommand <$> getChar
    case maybe_cmd of
      Nothing -> getNextCommand
      Just cmd -> return cmd

data GameCommand
  = WorldCommand !Command
  | Save
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

charToCommand :: Char -> Maybe GameCommand
charToCommand 'h' = Just (WorldCommand $ Move D8Left)
charToCommand 'k' = Just (WorldCommand $ Move D8Up)
charToCommand 'j' = Just (WorldCommand $ Move D8Down)
charToCommand 'l' = Just (WorldCommand $ Move D8Right)
charToCommand 'y' = Just (WorldCommand $ Move D8UpLeft)
charToCommand 'u' = Just (WorldCommand $ Move D8UpRight)
charToCommand 'b' = Just (WorldCommand $ Move D8DownLeft)
charToCommand 'n' = Just (WorldCommand $ Move D8DownRight)
charToCommand '4' = Just (WorldCommand $ Move D8Left)
charToCommand '8' = Just (WorldCommand $ Move D8Up)
charToCommand '2' = Just (WorldCommand $ Move D8Down)
charToCommand '6' = Just (WorldCommand $ Move D8Right)
charToCommand '7' = Just (WorldCommand $ Move D8UpLeft)
charToCommand '9' = Just (WorldCommand $ Move D8UpRight)
charToCommand '1' = Just (WorldCommand $ Move D8DownLeft)
charToCommand '3' = Just (WorldCommand $ Move D8DownRight)
charToCommand ' ' = Just (WorldCommand ActivateAura)
charToCommand 'S' = Just Save
charToCommand _ = Nothing

appearanceToCell :: ActorAppearance -> Square
appearanceToCell PlayerCharacter = Square '@' White True Black
appearanceToCell BeastFrog = Square 'F' Green True Black

itemAppearanceToCell :: ItemAppearance -> Square
itemAppearanceToCell BeastFrogCorpse = Square 'F' Red False Black

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

decorationToCell :: Decoration -> Square
decorationToCell NotDecorated = Square ' ' White False Black
decorationToCell (Spikes dir) = case dir of
  D8Up        -> Square '|'  Black True Black
  D8Down      -> Square '|'  Black True Black
  D8Left      -> Square '-'  Black True Black
  D8Right     -> Square '-'  Black True Black
  D8UpLeft    -> Square '\\' Black True Black
  D8UpRight   -> Square '/'  Black True Black
  D8DownRight -> Square '\\' Black True Black
  D8DownLeft  -> Square '/'  Black True Black
decorationToCell (BloodySpikes dir) = case dir of
  D8Up        -> Square '|'  Red False Black
  D8Down      -> Square '|'  Red False Black
  D8Left      -> Square '-'  Red False Black
  D8Right     -> Square '-'  Red False Black
  D8UpLeft    -> Square '\\' Red False Black
  D8UpRight   -> Square '/'  Red False Black
  D8DownRight -> Square '\\' Red False Black
  D8DownLeft  -> Square '/'  Red False Black

type ScreenCache = Map (V2 Int) Square

-- | Writes a level on the screen, at given offset.
--
-- This overwrites the whole screen.
writeLevel :: World
           -> ScreenCache
           -> String
           -> IO ScreenCache
writeLevel world cache username = do
  -- TODO: don't so wastefully write to terminal, we could get by with much
  -- less.
  setSGR [Reset]
  V2 tw th <- getWindowSize

  let (lvl, _, actor, actor_id) = currentLevelAndActor world
      level_title = lvl^.levelName

      set_cell_attributes cell =
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

            text_insert tcoords (Square _ fcolor vivid bcolor) (T.unpack -> txt) =
              void $ flip execStateT tcoords $ for_ txt $ \ch -> do
                current_coords <- get
                lift $ modifier (Square ch fcolor vivid bcolor) current_coords
                modify (+ V2 1 0)

        -- Clear everything

        for_ [0..th-1] $ \y ->
          for_ [0..tw-1] $ \x ->
            let tcoords = V2 x y
             in modifier (Square ' ' White False Black) tcoords

        -- HUD stuff

        text_insert (V2 1 2) (Square ' ' White True Black) (T.pack username)

        text_insert (V2 1 4) (Square ' ' White True Black) "Hitpoints:"
        case actor^.actorHitPoints of
          Nothing -> text_insert (V2 1 5)
                                 (Square ' ' Yellow True Black)
                                 "INVULNERABLE"
          Just hitp -> text_insert (V2 1 5)
                                 (Square ' ' (if | hitPointsCritical hitp
                                                   -> Red
                                                 | hitPointsHealthy hitp
                                                   -> Green
                                                 | otherwise
                                                   -> Yellow)
                                             True
                                             Black)
                                 (T.pack $ show (hitp^.hitPoints.hp) ++
                                           " / " ++
                                           show (hitp^.hitPoints.maxHp))

        -- Messages
        let msgs = currentMessages world
            insert_message [] _ = return ()
            insert_message (msg:rest) y | y <= 6 || y >= th-1 = return ()
                                        | otherwise = do
                                            let Message n txt = msg
                                            text_insert (V2 1 y)
                                                        (Square ' ' White False Black)
                                                        (if n > 1
                                                           then T.pack (show n) <> "x " <> txt
                                                           else txt)
                                            insert_message rest (y-1)

        insert_message msgs (th-2)

        -- The map

        let (w, h, access) = getCurrentFieldOfView world

        for_ [0..h-1] $ \y ->
          for_ [0..w-1] $ \x -> do
            let ox = tw - w + x
                oy = y + th `div` 2 - (h `div` 2)
                op = V2 ox oy
                lp = V2 (actor^.position._x + x - (w `div` 2))
                        (actor^.position._y + y - (h `div` 2))

            case access x y of
              (Just ap, _, _, _) -> modifier (appearanceToCell ap) op
              (_, _, dec, _) | dec /= NotDecorated -> modifier (decorationToCell dec) op
              (_, _, _, Just i) -> modifier (itemAppearanceToCell i) op
              (_, Just t, _, _)  -> modifier (featureToCell t) op
              _ -> case getMemoryAt actor_id lvl lp of
                Nothing -> return ()
                Just v  ->
                  modifier (let Square ch _ _ _ = featureToCell v
                             in Square ch Black True Black) op

        for_ (zip [0..] $ T.unpack level_title) $ \(x, ch) ->
          modifier (Square ch White True Black) (V2 x 0)

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

