{-# LANGUAGE DisambiguateRecordFields #-}

module Options where

import Data.Semigroup ((<>))
import Path
import Path.IO
import Options.Applicative

-- Helpers
infoh parser = info (helper <*> parser)
commandh name parser = command name . infoh parser
commandhd name parser = command name . infoh parser . progDesc
-- txtOption = option (pack <$> str)
strOptional = optional . strOption
-- txtOptional = optional . txtOption
manyStrOpts = many . strOption
-- manyTxtOpts = many . txtOption
lsh l s h = long l <> short s <> help h
lmh l m h = long l <> metavar m <> help h
lsmh l s m h = long l <> short s <> metavar m <> help h
-- manyArguments = many . argument auto . metavar
someArguments = some . argument str . metavar
-- strArgument = argument str . metavassr


data Position = Prepend | Append
data Zone     = UTC | Local

data Options = Options
  { position   :: Position  -- Whether datestamp goes before or after filename
  , zone       :: Zone      -- Time zone of datestamp
  , format     :: String    -- Format of datestamp, excluding separator
  , separator  :: String    -- Separator between datestamp and filename
  , optCommand :: Command
  , files      :: [FilePath]
  }

-- data Format = Format
--   { position  :: Position
--   , zone      :: Zone
--   , format    :: String
--   , separator :: String
--   }

data Command
  = Today
  | Modification
  | Access
  -- | Creation
  -- | Addition
  | Custom String
  | Remove


zoneP = switch (lsh "utc"   'z' "UTC time")   *> pure UTC
    <|> switch (lsh "local" 'l' "Local time") *> pure Local
    <|> pure UTC

formatP = strOption (lsmh "format" 'f' "FORMAT" "Datestamp formatting string (time lib)")
      <|> pure "%F"  -- use default?

separatorP = strOption (lsmh "separator" 's' "SEP" "Separator between datestamp and filename (default \" \")")
         <|> pure " "  -- use default?

positionP = switch (lsh "prepend" 'p' "Add datestamp before filename") *> pure Prepend
        <|> switch (lsh "append"  'a' "Add datestamp after filename")  *> pure Append
        <|> pure Prepend

commandP = subparser
  (  commandhd "today"        (pure Today)        "Stamp with today's date"
  <> commandhd "modification" (pure Modification) "Stamp with file modification date"
  <> commandhd "access"       (pure Access)       "Stamp with file addition date"
  <> commandhd "custom"       (Custom <$> strArgument (metavar "TEXT")) "Stamp with custom text"
  <> commandhd "remove"       (pure Remove)       "Remove datestamp"
  )

filesP = someArguments "FILES"

options = Options
  <$> positionP
  <*> zoneP
  <*> formatP
  <*> separatorP
  <*> commandP
  <*> filesP


-- | Global descriptions and options
descText = "datestamp is a tool for adding dates to file names."

description = fullDesc
    <> header "datestamp - a tool for adding dates to file names"
    <> progDesc descText
    <> footer "(c) 2017, Bjorn Buckwalter"

parseCommand :: IO Options
parseCommand = execParser (info (helper <*> options) description)
