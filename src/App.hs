{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as BM
import qualified Brick.Types as T
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core
import Control.Monad.IO.Class (liftIO)
import Data.FileEmbed (embedStringFile)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Engine
import Graphics.Vty as V hiding (Default)
import Text.Printf (printf)

data State = State
  { sWords :: [String],
    sWord :: String,
    sWordSize :: Int,
    sGuesses :: [Guess],
    sMaxGuesses :: Int,
    sInput :: String,
    sStatus :: String,
    sGameStatus :: GameStatus
  }
  deriving (Show, Read, Eq, Ord)

initState :: IO State
initState = do
  let wordSize = 5
      guessDict = $(embedStringFile "resource/dict/en-10k.txt")
      targetDict = $(embedStringFile "resource/dict/en-84k.txt")
  word <- pickWordFilter ((== wordSize) . length) . words $ guessDict
  return $
    State
      { sWords = words targetDict,
        sWord = word,
        sWordSize = wordSize,
        sGuesses = [],
        sMaxGuesses = 6,
        sInput = "",
        sStatus = "",
        sGameStatus = Ongoing
      }

appMain :: IO State
appMain = do
  BM.defaultMain app =<< initState

app :: BM.App State e ()
app =
  BM.App
    { BM.appDraw = draw,
      BM.appChooseCursor = BM.showFirstCursor,
      BM.appHandleEvent = handleEvent,
      BM.appStartEvent = return,
      BM.appAttrMap = const attrMap
    }

draw :: State -> [T.Widget n]
draw s =
  [ center . vBox $
      [ hBox
          [ border . padLeftRight 2 . padTopBottom 1 . vBox $
              [ hLimit guessWidth . hCenter . str $ "Guesses",
                drawGuesses (sGuesses s),
                drawGuesses futureGuesses
              ],
            str "  ",
            border . padRight (T.Pad 1) . padAll 1 . vLimit guessHeight . hLimit guessWidth . vBox $
              [ hCenter . str $ "Input",
                drawInput,
                padAll 1 . hCenter $ drawKeyboard,
                status,
                fill ' ',
                help
              ]
          ]
      ]
  ]
  where
    guessWidth = 5 * sWordSize s
    guessHeight = 3 * sWordSize s + 4
    status = strWrap . sStatus $ s
    drawGuesses ats = vBox . map drawGuess $ ats
    drawGuess = hBox . map drawChar
    drawChar :: (Char, GuessType) -> T.Widget n
    drawChar c = charAttr (snd c) . border . padRight (T.Pad 1) . padLeft (T.Pad 1) . str . (: []) . fst $ c
    charAttr gt = case gt of
      NotInWord -> withAttr notInWordAttr
      WrongSpot -> withAttr wrongSpotAttr
      CorrectSpot -> withAttr correctSpotAttr
      Default -> id
    futureGuesses = map (const futureGuess) [0 .. sMaxGuesses s - length (sGuesses s) - 1]
    futureGuess = map (const (' ', Default)) [0 .. sWordSize s - 1]
    drawInput = drawGuess . map (,Default) . (\cs -> cs ++ replicate (sWordSize s - length cs) ' ') . sInput $ s
    guessedMap = M.fromList . nub . concat . sGuesses $ s
    keys =
      map
        ( padLeft (T.Pad 1)
            . (\c -> charAttr (fromMaybe Default $ guessedMap M.!? c) $ str [c])
        )
        "qwertyuiopasdfghjklzxcvbnm"
    drawKeyboard =
      vBox
        [ hBox . take 10 $ keys,
          padLeft (T.Pad 1) . hBox . take 9 . drop 10 $ keys,
          padLeft (T.Pad 2) . hBox . take 7 . drop 19 $ keys
        ]
    help =
      withAttr secondaryTextAttr . vBox . map (\(c, a) -> hBox [str c, vLimit 1 $ fill ' ', str a]) $
        [ ("C-c", "quit"),
          ("C-r", "give up"),
          ("ent", "submit guess"),
          ("r  ", "new game")
        ]

handleEvent :: State -> T.BrickEvent n e -> T.EventM n (T.Next State)
handleEvent s e = case sGameStatus s of
  Ongoing -> case e of
    T.VtyEvent ve -> case ve of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt s
      V.EvKey (V.KChar 'l') [V.MCtrl] -> clearH
      V.EvKey (V.KChar 'r') [V.MCtrl] -> giveUpH
      V.EvKey (V.KChar k) [] -> inputH k
      V.EvKey V.KBS [] -> bspcH
      V.EvKey V.KEnter [] -> guessH
      _ -> BM.continue s
    _ -> BM.continue s
    where
      clearH = BM.continue $ s {sInput = ""}
      giveUpH = BM.continue $ s {sGameStatus = Loss, sStatus = printf "You lose, the word was \"%s\"" w}
      inputH k = BM.continue $ s {sInput = take (sWordSize s) $ sInput s ++ [k]}
      bspcH = BM.continue $ s {sInput = if null $ sInput s then "" else init $ sInput s}
      guessH = do
        let s' = s {sInput = ""}
            ns
              | length g /= length w = s' {sStatus = printf "Word size must be %d" $ length w}
              | not $ isCorrectWord g ws = s' {sStatus = printf "\"%s\" is not a valid word" g}
              | otherwise =
                case (g == w, length (sGuesses s'') == sMaxGuesses s'') of
                  (True, _) -> s'' {sGameStatus = Win, sStatus = "You guessed the word!"}
                  (_, True) -> s'' {sGameStatus = Loss, sStatus = printf "Wrong guess, the word was \"%s\"" w}
                  (_, False) -> s''
              where
                s'' = s' {sGuesses = sGuesses s ++ [attempt], sInput = ""}
        BM.continue ns
  _ -> case e of
    T.VtyEvent ve -> case ve of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt s
      V.EvKey (V.KChar 'r') [] -> BM.continue =<< liftIO initState
      _ -> BM.continue s
    _ -> BM.continue s
  where
    g = sInput s
    w = sWord s
    ws = sWords s
    attempt = guess g w

attrMap :: A.AttrMap
attrMap =
  A.attrMap
    V.defAttr
    [ (notInWordAttr, fg V.brightBlack),
      (wrongSpotAttr, fg V.yellow),
      (correctSpotAttr, fg V.green),
      (secondaryTextAttr, fg V.brightBlack)
    ]

notInWordAttr :: A.AttrName
notInWordAttr = "notInWord"

wrongSpotAttr :: A.AttrName
wrongSpotAttr = "wrongSpot"

correctSpotAttr :: A.AttrName
correctSpotAttr = "correctSpot"

secondaryTextAttr :: A.AttrName
secondaryTextAttr = "secondaryText"
