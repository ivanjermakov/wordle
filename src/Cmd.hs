{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cmd where

import Control.Monad (when)
import Data.FileEmbed (embedStringFile)
import System.Console.Docopt
import System.Environment (getArgs)

data Settings = Settings
  { argTargetDict :: [String],
    argGuessDict :: [String],
    argMaxGuesses :: Int,
    argWordSize :: Int
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

-- TODO: validation
-- TODO: unicode/ascii result
-- TODO: quiet option to suppress result
argSettings :: IO Settings
argSettings = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` longOption "help") $ do
    exitWithUsage patterns

  let targetDict = $(embedStringFile "resource/dict/en-10k.txt")
      guessDict = $(embedStringFile "resource/dict/en-84k.txt")
  td <- maybeReadOptionDict (words targetDict) 't' args
  gd <- maybeReadOptionDict (words guessDict) 'g' args

  let attempts = maybe 6 read . getArg args . shortOption $ 'a'
      wordLength = maybe 5 read . getArg args . shortOption $ 'l'
      settings =
        Settings
          { argTargetDict = td,
            argGuessDict = gd,
            argMaxGuesses = attempts,
            argWordSize = wordLength
          }

  return settings
