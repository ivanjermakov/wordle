{-# LANGUAGE TupleSections #-}

module Engine where

import Data.List (elemIndex)
import System.Random

data GuessType = NotInWord | WrongSpot | CorrectSpot | Default
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
guess guess word
  = zip guess $ zipWith max perfects corrects
    where perfects    = zipWith (\c g -> if c == g then CorrectSpot else NotInWord) word guess
          corrects    = go word' guess'
          word'       = zipWith (\c p -> if p == CorrectSpot then Nothing else Just c) word perfects
          guess'      = zipWith (\g p -> if p == CorrectSpot then Nothing else Just g) guess perfects
          go c []     = []
          go c (g:gs) =
            case (g, elemIndex g c) of
              (Nothing, _)  -> NotInWord : go c gs
              (_, Nothing)  -> NotInWord : go c gs
              (_, Just idx) -> WrongSpot : go c' gs
                where c' = setAt c idx Nothing

{- | 'setAt' @xs i x@ replaces the element at index @i@
in list @xs@ with value @x@.
-}
setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs
