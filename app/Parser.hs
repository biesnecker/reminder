module Parser (parseEntry) where

import           Control.Applicative (Alternative((<|>)))
import           Control.Monad (void)
import           Data.Attoparsec.Text
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Data.Time (Day(..), fromGregorian, toGregorian)
import           Entry

data WildcardOrNumber = Wildcard
                      | Number Int

wildcardOrDigits :: Int -> Parser WildcardOrNumber
wildcardOrDigits n =
  let wc = do
        void $ char '*'
        return Wildcard
      cd = do
        x <- count n digit
        return $ Number (read x)
  in wc <|> cd

dateParser :: Parser (WildcardOrNumber, WildcardOrNumber, WildcardOrNumber)
dateParser = do
  y <- wildcardOrDigits 4
  dash
  m <- wildcardOrDigits 2
  dash
  d <- wildcardOrDigits 2
  return $ (y, m, d)
  where
    dash = void $ char '-'

entryParser :: (Day, Day) -> Parser [Entry]
entryParser range = do
  (y, m, d) <- dateParser
  skipSpace
  r <- takeText
  endOfInput
  return $ fromWildcards range y m d r

fromWildcards :: (Day, Day)
              -> WildcardOrNumber
              -> WildcardOrNumber
              -> WildcardOrNumber
              -> Text
              -> [Entry]
fromWildcards (start, end) (Number y) (Number m) (Number d) txt =
  let singleDate = fromGregorian (fromIntegral y) m d
  in [Entry singleDate txt | singleDate >= start && singleDate <= end]
fromWildcards (start, end) y m d txt = mapMaybe go [start .. end]
  where
    matchWildcard wc n = case wc of
      Wildcard -> Just n
      Number x -> if (fromIntegral x) == n
                  then Just n
                  else Nothing

    go day = do
      let (cy, cm, cd) = toGregorian day
      my <- matchWildcard y cy
      mm <- matchWildcard m cm
      md <- matchWildcard d cd
      return $ Entry (fromGregorian my mm md) txt

parseEntry :: (Day, Day) -> Text -> Maybe [Entry]
parseEntry range t = case parseOnly (entryParser range) t of
  Left _  -> Nothing
  Right r -> Just r
