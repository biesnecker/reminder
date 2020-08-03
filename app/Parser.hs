module Parser where

import           Control.Monad        (void)
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import           Data.Time            (Day (..), fromGregorian)
import           Entry

dateParser :: Parser Day
dateParser = do
  y <- cd 4
  dash
  m <- cd 2
  dash
  d <- cd 2
  return $ fromGregorian (read y) (read m) (read d)
  where
    cd = flip count digit
    dash = void $ char '-'

entryParser :: Parser Entry
entryParser = do
  d <- dateParser
  skipSpace
  r <- takeText
  endOfInput
  return $ Entry d r

parseEntry :: Text -> Maybe Entry
parseEntry t =
  case parseOnly entryParser t of
    Left _  -> Nothing
    Right r -> Just r
