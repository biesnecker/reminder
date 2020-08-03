{-# LANGUAGE RecordWildCards #-}

module Entry
  ( Entry(..)
  , filterEntry
  ) where

import           Data.Text (Text, unpack)
import           Data.Time (Day, addDays, defaultTimeLocale, formatTime)

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

filterEntry :: Day -> Int -> Int -> Entry -> Bool
filterEntry today before after entry =
  let bd = addDays (fromIntegral $ (-1) * before) today
      ad = addDays (fromIntegral $ after) today
      ed = entryDate entry
   in ed >= bd && ed <= ad
