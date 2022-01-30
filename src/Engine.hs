{-# LANGUAGE TupleSections #-}

module Engine where

import System.Random

data GuessType = NotInWord | WrongSpot | CorrectSpot | Default
  deriving (Show, Read, Eq, Ord)

type Attempt = [(Char, GuessType)]

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

guess :: String -> String -> Attempt
guess g w = zipWith (curry f) g w
  where
    f (gc, c) = (gc,) $ case (c == gc, gc `elem` w) of
      (True, _) -> CorrectSpot
      (_, True) -> WrongSpot
      (_, False) -> NotInWord
