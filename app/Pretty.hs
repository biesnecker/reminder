{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( printColor
  , printBW
  ) where

import           Data.Bifunctor (bimap)
import           Data.Time      (Day)
import           Entry
import           Rainbow

printColor :: Day -> [Entry] -> IO ()
printColor today entries =
  mapM_ putChunksLn $
  map (chunksToList . modifyChunkColor today . chunkify) entries

printBW :: [Entry] -> IO ()
printBW = mapM_ putStrLn . map show

chunkify :: Entry -> (Day, (Chunk, Chunk))
chunkify e =
  let (d, r) = entryParts e
   in (entryDate e, (chunk d, chunk r))

modifyChunkColor :: Day -> (Day, (Chunk, Chunk)) -> (Chunk, Chunk)
modifyChunkColor today (date, parts) =
  let modifier =
        if | date == today -> present
           | date > today -> future
           | otherwise -> past
   in uncurry bimap modifier parts

chunksToList :: (Chunk, Chunk) -> [Chunk]
chunksToList (a, b) = [a, chunk " ", b]

past :: ((Chunk -> Chunk), (Chunk -> Chunk))
past = (fore grey, fore grey)

present :: ((Chunk -> Chunk), (Chunk -> Chunk))
present = (fore brightYellow, fore brightYellow)

future :: ((Chunk -> Chunk), (Chunk -> Chunk))
future = (fore yellow . faint, fore yellow . faint)