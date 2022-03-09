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
readWords = lines <$> readFile

pickWord :: [String] -> IO String
pickWord = pickWordFilter $ const True

pickWordFilter :: (String -> Bool) -> [String] -> IO String
pickWordFilter f ws = do
  let fws = filter f ws
  n <- randomRIO (0, length fws - 1)
  return $ fws !! n

pickDailyWord :: Day -> [String] -> IO String
pickDailyWord day dict = do
  ix <- dailyWordIndex day
  return $ dict !! (ix `mod` length dict)

dailyWordIndex :: Day -> IO Int
dailyWordIndex currentDay = do
  -- January 1, 2022 Game Epoch
  let startDay = parseTimeOrError True defaultTimeLocale "%F" "2022-01-01"
  let sinceDays = diffDays currentDay startDay
  return . fromIntegral . (+ 195) $ sinceDays

isCorrectWord :: String -> [String] -> Bool
isCorrectWord = elem

guess :: String -> String -> Guess
guess gWord tWord = foldl f [] $ zip gWord tWord
  where
    f :: Guess -> (Char, Char) -> Guess
    f acc (g, t) = acc ++ [(g, guessType)]
      where
        guessType
          | g == t = CorrectSpot
          | targetTotal g - correctTotal g - wrongBefore g > 0 = WrongSpot
          | otherwise = NotInWord
        wrongBefore c = length . filter (== c) . map fst . filter ((== WrongSpot) . snd) $ acc
        targetTotal c = length . filter (== c) $ tWord
        correctTotal c = length . filter (\(a, b) -> a == c && b == c) $ zip gWord tWord

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
