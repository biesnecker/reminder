{-# LANGUAGE RecordWildCards #-}

module Entry (Entry(..), entryParts) where

import           Data.Text (Text, pack, unpack)
import           Data.Time (Day, defaultTimeLocale, formatTime)

data Entry = Entry { entryDate :: Day, entryReminder :: Text }

instance Show Entry where
  show entry = let (d, r) = entryParts entry
               in unwords $ map unpack [d, r]

entryParts :: Entry -> (Text, Text)
entryParts Entry { .. } =
  let dateText = pack $ formatTime defaultTimeLocale "%F" entryDate
  in (dateText, entryReminder)
