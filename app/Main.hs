{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List (sortOn)
import qualified Data.Text as T
import           Data.Time (Day, LocalTime(LocalTime), ZonedTime(ZonedTime)
                          , addDays, getZonedTime)
import           Entry
import           Options
import           Parser
import           Pretty
import qualified Streaming.Prelude as S
import           System.IO (Handle, IOMode(ReadMode), withFile)

entriesFromHandle :: (Day, Day) -> Handle -> IO [Entry]
entriesFromHandle range hdl = S.toList_
  $ S.concat
  $ S.mapMaybe (parseEntry range)
  $ S.map T.pack
  $ S.fromHandle hdl

today :: IO Day
today = do
  (ZonedTime (LocalTime d _) _) <- getZonedTime
  return d

dateRange :: Day -> Int -> Int -> (Day, Day)
dateRange start back forward =
  let bd = addDays (fromIntegral $ (-1) * back) start
      ad = addDays (fromIntegral forward) start
  in (bd, ad)

main :: IO ()
main = withOptions
  $ \Options { .. } -> withFile path ReadMode
  $ \handle -> do
    td <- today
    entries <- sortOn entryDate
      <$> entriesFromHandle (dateRange td lookBack lookAhead) handle
    if noColor
      then printBW entries
      else printColor td entries
