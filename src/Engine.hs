{-# LANGUAGE TupleSections #-}

module Engine where

import Data.List (intercalate)
import System.Random

data GuessType = Default | NotInWord | WrongSpot | CorrectSpot
  deriving (Show, Read, Eq, Ord)

type Guess = [(Char, GuessType)]

data GameStatus = Win | Loss | Ongoing
  deriving (Show, Read, Eq, Ord)

readWords :: FilePath -> IO [String]
readWords p = lines <$> readFile p

pickWord :: [String] -> IO String
pickWord = pickWordFilter $ const True

pickWordFilter :: (String -> Bool) -> [String] -> IO String
pickWordFilter f ws = do
  let fws = filter f ws
  n <- randomRIO (0, length fws - 1)
  return $ fws !! n

isCorrectWord :: String -> [String] -> Bool
isCorrectWord = elem

guess :: String -> String -> Guess
guess g w = zipWith (curry f) g w
  where
    f (gc, c) = (gc,) $ case (c == gc, gc `elem` w) of
      (True, _) -> CorrectSpot
      (_, True) -> WrongSpot
      (_, False) -> NotInWord

showResultGrid :: Bool -> [Guess] -> String
showResultGrid isUnicode = intercalate "\n" . map (concatMap (f . snd))
  where
    f = if isUnicode then showGuessTypeUnicode else showGuessTypeAscii

showGuessTypeUnicode :: GuessType -> String
showGuessTypeUnicode s = case s of
  Default -> " "
  NotInWord -> "⬜"
  WrongSpot -> "🟨"
  CorrectSpot -> "🟩"

showGuessTypeAscii :: GuessType -> String
showGuessTypeAscii s = case s of
  Default -> " "
  NotInWord -> "."
  WrongSpot -> "w"
  CorrectSpot -> "c"
