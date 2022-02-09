{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cmd where

import Control.Monad (when)
import Data.FileEmbed (embedStringFile)
import Engine
import System.Console.Docopt
import System.Environment (getArgs)

data Settings = Settings
  { argTargetDict :: [String],
    argGuessDict :: [String],
    argDailyDict :: [String],
    argMaxGuesses :: Int,
    argWordSize :: Int,
    argGameMode :: GameMode,
    argUnicode :: Bool
  }
  deriving (Show, Read, Eq, Ord)

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

maybeReadOptionDict :: [String] -> Char -> Arguments -> IO [String]
maybeReadOptionDict def o args =
  maybe (return def) (fmap words . readFile)
    . getArg args
    . shortOption
    $ o

-- TODO: quiet option to suppress result
argSettings :: IO Settings
argSettings = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` longOption "help") $ do
    exitWithUsage patterns

  let targetDict = $(embedStringFile "resource/dict/en-10k.txt")
      guessDict = $(embedStringFile "resource/dict/en-84k.txt")
      dailyDict = words $(embedStringFile "resource/dict/official.txt")
  td <- maybeReadOptionDict (words targetDict) 't' args
  gd <- maybeReadOptionDict (words guessDict) 'g' args

  let attempts = maybe 6 read . getArg args . shortOption $ 'a'
      wordLength = maybe 5 read . getArg args . shortOption $ 'l'
      gameMode = (\b -> if b then Daily else Infinite) . isPresent args . shortOption $ 'd'
      unicode = not . isPresent args . shortOption $ 's'
      settings =
        Settings
          { argTargetDict = td,
            argGuessDict = gd,
            argDailyDict = dailyDict,
            argMaxGuesses = attempts,
            argWordSize = wordLength,
            argGameMode = gameMode,
            argUnicode = unicode
          }

  return settings
