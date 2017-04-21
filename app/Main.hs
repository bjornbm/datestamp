{-
TODO:

+ Add a "remove" command which strips a date.
+ Add a "toggle" command which add or strips depending if file already stamped.
+ Proper command line options:
    + Custom date/time format
    + Custom separator between stamp and original name
    + Local time timestamps?
- Verbosity flag

-}

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((<=<))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime --(utcToLocalZonedTime)
import Lib
import Path
import Path.IO
import System.Directory (renameDirectory)
import System.Environment
import qualified System.FilePath as FP (replaceBaseName, takeBaseName)
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



-- | Version of filepath's takeBaseName that works on Paths.
takeBaseName :: Path Abs File -> String
takeBaseName = FP.takeBaseName . toFilePath

-- | Version of filepath's replaceBaseName that works on Paths.
replaceBaseName :: Path Abs File -> String -> IO (Path Abs File)
replaceBaseName old newbasename = parseAbsFile (FP.replaceBaseName (toFilePath old) newbasename)

-- | Replace the basename of a file in the file system.
replaceBaseName' :: Path Abs File -> String -> IO ()
replaceBaseName' old newbasename = rename old =<< replaceBaseName old newbasename

-- | Rename a file. The file may actually be a directory too!
rename :: Path Abs File -> Path Abs File -> IO ()
rename old new = isLocationOccupied new >>= \case
  True  -> error $ "File aleady exists: " ++ show new
  False -> doesFileExist old >>= \case
    True  -> renameFile old new
    False -> renameDirectory (toFilePath old) (toFilePath new) -- A directory!


-- | Stamp a file name with the given string according to the provided
  -- formatting options.
addStamp :: Format -> String -> Path Abs File -> IO ()
addStamp fmt stamp old = replaceBaseName' old $
    concat' [stamp, separator fmt, takeBaseName old]
  where
    concat' = case position fmt of
        Prepend -> concat
        Append  -> concat . reverse

-- | Stamp a file with the given date according to the provided formatting
  -- options.
stampDate :: Format -> UTCTime -> Path Abs File -> IO ()
stampDate fmt time file = case zone fmt of
  UTC   -> addStamp' time
  Local -> addStamp' . zonedTimeToLocalTime =<< utcToLocalZonedTime time
  where
    addStamp' t = if toggle fmt
      then toggleStamp fmt (formatTime defaultTimeLocale (formatStr fmt) t) file
      else    addStamp fmt (formatTime defaultTimeLocale (formatStr fmt) t) file


-- | Stamp a file with today's date according to the provided formatting
  -- options.
stampToday :: Format -> Path Abs File -> IO ()
stampToday fmt file = getCurrentTime >>= flip (stampDate fmt) file

-- | Stamp a file with the date obtained from the given action according
  -- to the provided formatting options.
stampFileDate :: Format -> (Path Abs File -> IO UTCTime) -> Path Abs File -> IO ()
stampFileDate fmt f file = f file >>= flip (stampDate fmt) file


-- | Removes a stamp from a basename, if a stamp of correct format exists
  -- and is separated from a non-empty remainder of the base name. The stamp
  -- format and the separator are defined by the provided formatting options.
stripStamp :: Format -> String -> Maybe String
stripStamp fmt basename = case position fmt of
  Prepend -> do
    let (stamp, rest) = splitAtFirst (separator fmt) basename
    parseTimeM False defaultTimeLocale (formatStr fmt) stamp :: Maybe UTCTime
    return rest
  Append -> do
    let (stampr, restr) = splitAtFirst (reverse $ separator fmt) (reverse basename)
    parseTimeM False defaultTimeLocale (formatStr fmt) (reverse stampr) :: Maybe UTCTime
    return (reverse restr)

-- | splitAtFirst ", " "a, b, c" == ("a", "b, c")
splitAtFirst :: String -> String -> (String, String)
splitAtFirst = splitAtFirst' ""
  where
    splitAtFirst' :: String -> String -> String -> (String, String)
    splitAtFirst' cs sep [] = (cs, "")
    splitAtFirst' cs sep string = case stripPrefix sep string of
      Nothing -> splitAtFirst' (cs ++ [head string]) sep $ tail string
      Just s  -> (cs, s)


-- | Toggle a datestamp on a file name. If the file already has a datestamp
  -- the datestamp is removed. If there is no datestamp a datestamp is added.
  -- What to add/remove is decided by the provided formatting options.
toggleStamp :: Format -> String -> Path Abs File -> IO ()
toggleStamp fmt stamp old = case stripStamp fmt (takeBaseName old) of
  Nothing          -> addStamp fmt stamp old
  Just newbasename -> replaceBaseName' old newbasename

-- | Remove a stamp from a file name. What to remove is decided by the
  -- provided formatting options.
removeStamp :: Format -> Path Abs File -> IO ()
removeStamp fmt old = case stripStamp fmt (takeBaseName old) of
  Nothing          -> return ()
  Just newbasename -> replaceBaseName' old newbasename
