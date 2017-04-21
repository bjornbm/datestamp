{-
TODO:

- Add a "remove" option which strips a date.
- Proper command line options:
    - Custom date/time format
    - Custom separator between stamp and original name
    - Verbosity flag
    - Local time timestamps?

-}

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((<=<))
import Data.Time.Clock
import Data.Time.Format
import Lib
import Path
import Path.IO
import System.Directory (renameDirectory)
import System.Environment
import Options


main :: IO ()
main = do
  options <- parseCommand
  files <- mapM resolveFile' $ files options
  case optCommand options of
    Today         -> mapM_ prependToday files
    Modification  -> mapM_ (prependFileDate getModificationTime) files
    Access        -> mapM_ (prependFileDate getAccessTime) files
    Custom custom -> mapM_ (prepend custom) files
    Remove        -> undefined


prepend :: String -> Path Abs File -> IO ()
prepend pre old = do
  new <- parent old </$> parseRelFile (pre ++ sep ++ toFilePath (filename old))
  rename old new
  where
    sep = " "  -- TODO Should come from options.
    dir </$> file = (dir </>) <$> file

rename :: Path Abs File -> Path Abs File -> IO ()
rename old new = isLocationOccupied new >>= \case
  True  -> error $ "File aleady exists: " ++ show new
  False -> doesFileExist old >>= \case
    True  -> renameFile old new
    False -> renameDirectory (toFilePath old) (toFilePath new) -- A directory!

prependToday :: Path Abs File -> IO ()
prependToday file = getCurrentTime >>= flip prependDate file

prependFileDate :: (Path Abs File -> IO UTCTime) -> Path Abs File -> IO ()
prependFileDate f file = f file >>= flip prependDate file

prependDate :: UTCTime -> Path Abs File -> IO ()
prependDate time = prepend (formatTime defaultTimeLocale "%F" time)  -- TODO should use formatTime
