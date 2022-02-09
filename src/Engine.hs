module Engine where

import Data.List (intercalate)
import Data.Time
import System.Random

data GuessType = Default | NotInWord | WrongSpot | CorrectSpot
  deriving (Show, Read, Eq, Ord)

type Guess = [(Char, GuessType)]

data GameStatus = Win | Loss | Ongoing
  deriving (Show, Read, Eq, Ord)

data GameMode = Daily | Infinite
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

pickWordDaily :: [String] -> IO String
pickWordDaily d = do
  ix <- todayIndex
  return $ d !! (ix `mod` length d)

todayIndex :: IO Int
todayIndex = do
  currentDay <- utctDay <$> getCurrentTime
  -- January 1, 2022 Game Epoch
  let startDay = parseTimeOrError True defaultTimeLocale "%F" "2022-01-01"
  let sinceDays = diffDays currentDay startDay
  return . fromIntegral . (+ 195) $ sinceDays

isCorrectWord :: String -> [String] -> Bool
isCorrectWord = elem

guess :: String -> String -> Guess
guess gWord tWord = zipWith3 f gWord tWord [0 ..]
  where
    f :: Char -> Char -> Int -> (Char, GuessType)
    f g t i
      | g == t = (g, CorrectSpot)
      | g `notElem` tWord = (g, NotInWord)
      | countInTargetCorrect g > countBefore g = (g, NotInWord)
      | countTotal g > countBefore g = (g, WrongSpot)
      | otherwise = (g, NotInWord)
      where
        countInTarget c = length . filter (== c)
        countTotal = flip countInTarget tWord
        countBefore c = countInTarget c . take (i - 1) $ tWord
        countInTargetCorrect c = length . filter (\(a, b) -> a == c && b == c) $ zip gWord tWord

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
