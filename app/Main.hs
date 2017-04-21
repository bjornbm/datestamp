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
import Data.Time.LocalTime --(utcToLocalZonedTime)
import Lib
import Path
import Path.IO
import System.Directory (renameDirectory)
import System.Environment
import System.FilePath (replaceBaseName, takeBaseName)
import Options


main :: IO ()
main = do
  options <- parseCommand
  files <- mapM resolveFile' $ files options
  let fmt = format options
  case optCommand options of
    Today        -> mapM_ (stampToday fmt) files
    Modification -> mapM_ (stampFileDate fmt getModificationTime) files
    Access       -> mapM_ (stampFileDate fmt getAccessTime) files
    Custom stamp -> mapM_ (addStamp fmt stamp) files
    Remove       -> mapM_ (removeStamp fmt) files


addStamp :: Format -> String -> Path Abs File -> IO ()
addStamp format stamp old = do
  new <- parseAbsFile $ replaceBaseName (toFilePath old) $ concat' [stamp, sep, basename]
  rename old new
  where
    basename = takeBaseName $ toFilePath old
    sep = separator format
    concat' = case position format of
        Prepend -> concat
        Append  -> concat . reverse

rename :: Path Abs File -> Path Abs File -> IO ()
rename old new = isLocationOccupied new >>= \case
  True  -> error $ "File aleady exists: " ++ show new
  False -> doesFileExist old >>= \case
    True  -> renameFile old new
    False -> renameDirectory (toFilePath old) (toFilePath new) -- A directory!

stampToday :: Format -> Path Abs File -> IO ()
stampToday fmt file = getCurrentTime >>= flip (stampDate fmt) file

stampFileDate :: Format -> (Path Abs File -> IO UTCTime) -> Path Abs File -> IO ()
stampFileDate fmt f file = f file >>= flip (stampDate fmt) file

stampDate :: Format -> UTCTime -> Path Abs File -> IO ()
stampDate fmt time file = case zone fmt of
  UTC   -> addStamp' time
  Local -> addStamp' . zonedTimeToLocalTime =<< utcToLocalZonedTime time
  where
    addStamp' t = addStamp fmt (formatTime defaultTimeLocale (formatStr fmt) t) file

removeStamp :: Format -> Path Abs File -> IO ()
removeStamp fmt file = undefined
