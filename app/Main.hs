{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List         (sortOn)
import qualified Data.Text         as T
import qualified Data.Time         as Tm
import           Entry
import           Options
import           Parser
import qualified Streaming.Prelude as S
import           System.IO         (Handle, IOMode (ReadMode), withFile)

filteredEntriesFromHandle :: (Entry -> Bool) -> Handle -> IO [Entry]
filteredEntriesFromHandle f hdl =
  S.toList_ $
  S.filter f $ S.mapMaybe parseEntry $ (S.map T.pack) $ S.fromHandle hdl

main :: IO ()
main =
  withOptions $ \Options {..} ->
    withFile path ReadMode $ \handle -> do
      td <- today
      let filterF = filterEntry td lookBack lookAhead
      entries <- sortOn entryDate <$> filteredEntriesFromHandle filterF handle
      mapM_ putStrLn $ map show entries
  where
    today = do
      (Tm.ZonedTime (Tm.LocalTime d _) _) <- Tm.getZonedTime
      return d
