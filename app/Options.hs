module Options
  ( Options(..)
  , withOptions
  ) where

import           Options.Applicative

data Options =
  Options
    { path      :: FilePath
    , lookAhead :: Int
    , lookBack  :: Int
    }
  deriving (Show, Eq)

op :: Parser Options
op =
  Options <$> argument str (metavar "FILE") <*>
  option
    auto
    (long "lookahead" <>
     short 'a' <> help "How many days ahead to surface reminders" <> value 3) <*>
  option
    auto
    (long "lookback" <>
     short 'b' <>
     help "How many days to surface reminders after they've passed" <> value 0)

withOptions :: (Options -> IO a) -> IO a
withOptions f = execParser opts >>= f
  where
    opts =
      info
        (op <**> helper)
        (fullDesc <>
         progDesc "Shows reminders on the commandline" <>
         header "reminder - so you don't forget")
