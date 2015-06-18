-- | Debugging functions.

module RWPAS.Debug
  ( logShow )
  where

import Control.Concurrent
import System.IO
import System.IO.Unsafe

logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()
{-# NOINLINE logLock #-}

logShow :: Show a => a -> b -> b
logShow thing result = unsafePerformIO $ withMVar logLock $ \_ -> do
  withFile "debug_log.txt" AppendMode $ \handle ->
    hPrint handle thing
  return result

