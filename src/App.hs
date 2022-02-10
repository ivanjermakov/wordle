{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as BM
import qualified Brick.Types as T
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core
import Cmd
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time
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
    sGameStatus :: GameStatus,
    sGameMode :: GameMode,
    sResults :: [String],
    sUnicode :: Bool,
    sWordIx :: String
  }
  deriving (Show, Read, Eq, Ord)

-- TODO: notify when daily was already played
initState :: IO State
initState = do
  ss <- argSettings
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  ix <- (+ 1) <$> dailyWordIndex today
  let gm = argGameMode ss
      showIx = if gm == Daily then show ix else "∞"
  word <-
    if gm == Infinite
      then pickWordFilter ((== argWordSize ss) . length) . argTargetDict $ ss
      else pickDailyWord today . argDailyDict $ ss
  return $
    State
      { sWords = argGuessDict ss,
        sWord = word,
        sWordSize = argWordSize ss,
        sGuesses = [],
        sMaxGuesses = argMaxGuesses ss,
        sInput = "",
        sStatus = "Wordle " ++ showIx,
        sGameStatus = Ongoing,
        sGameMode = gm,
        sResults = [],
        sUnicode = argUnicode ss,
        sWordIx = showIx
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
  [ center . vLimit guessHeight . vBox $
      [ hBox
          [ border . padLeft (T.Pad 2) . padRight (T.Pad 1) . padBottom T.Max . hLimit guessWidth . hCenter . vBox . map hCenter $
              [ str "Guesses",
                drawGuesses (sGuesses s),
                drawGuesses futureGuesses
              ],
            str "  ",
            border . padLeft (T.Pad 1) . padRight (T.Pad 2) . padBottom T.Max . hLimit (max 20 guessWidth) . vBox . map hCenter $
              [ str "Input",
                drawInput,
                drawKeyboard,
                vLimit 1 . fill $ ' ',
                status,
                fill ' ',
                padLeft (T.Pad 1) help
              ]
          ]
      ]
  ]
  where
    guessWidth = 5 * sWordSize s + 1
    guessHeight = maximum [17, 3 * sMaxGuesses s + 3]
    status = withAttr wrongSpotAttr . padLeft (T.Pad 1) . strWrap . sStatus $ s
    drawGuesses = vBox . map drawGuess
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
    drawInput = padLeft (T.Pad 1) . drawGuess . map (,Default) . (\cs -> cs ++ replicate (sWordSize s - length cs) ' ') . sInput $ s
    guessedMap = M.fromListWith max . nub . concat . sGuesses $ s
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
        let ns
              | length g /= length w = s {sStatus = printf "Word size must be %d" $ length w}
              | not $ isCorrectWord g ws = s {sStatus = printf "\"%s\" is not a valid word" g}
              | otherwise =
                case (g == w, length (sGuesses s') == sMaxGuesses s') of
                  (True, _) ->
                    s'
                      { sGameStatus = Win,
                        sStatus = "You guessed the word!",
                        sResults = sResults s' ++ [showResult s']
                      }
                  (_, True) ->
                    s'
                      { sGameStatus = Loss,
                        sStatus = printf "Wrong guess, the word was \"%s\"" w
                      }
                  (_, False) -> s' {sStatus = ""}
              where
                s' = s {sGuesses = sGuesses s ++ [attempt], sInput = ""}
                showResult _s = intercalate "\n" (t : "" : grid)
                  where
                    t = unwords ["Wordle", n, att]
                    n = sWordIx _s
                    att = concat [show . length . sGuesses $ _s, "/", show . sMaxGuesses $ _s]
                    grid = sResults _s ++ [showResultGrid (sUnicode _s) . sGuesses $ _s]
        BM.continue ns {sInput = ""}
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
