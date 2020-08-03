{-# LANGUAGE RecordWildCards #-}

module Entry
  ( Entry(..)
  ) where

import           Data.Text (Text, unpack)
import           Data.Time (Day, defaultTimeLocale, formatTime)

data Entry =
  Entry
    { entryDate     :: Day
    , entryReminder :: Text
    }

instance Show Entry where
  show Entry {..} =
    let dateString = formatTime defaultTimeLocale "%F" entryDate
        reminderString = unpack entryReminder
     in unwords [dateString, reminderString]
