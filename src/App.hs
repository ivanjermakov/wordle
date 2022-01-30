{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (hBox, padLeft, padRight, str, vBox, withAttr)
import Control.Monad.IO.Class (liftIO)
import Engine
import Graphics.Vty as V hiding (Default)
import Text.Printf (printf)

data State = State
  { sWords :: [String],
    sWord :: String,
    sWordSize :: Int,
    sAttempts :: [Attempt],
    sMaxAttempts :: Int,
    sInput :: String,
    sStatus :: String,
    sGameStatus :: GameStatus
  }
  deriving (Show, Read, Eq, Ord)

initState :: IO State
initState = do
  let wordSize = 5
  ws <- readWords "resource/dict/en-10k.txt"
  word <- pickWordFilter ((== wordSize) . length) ws
  return $
    State
      { sWords = ws,
        sWord = word,
        sWordSize = wordSize,
        sAttempts = [],
        sMaxAttempts = 6,
        sInput = "",
        sStatus = "",
        sGameStatus = Ongoing
      }

appMain :: IO State
appMain = do
  M.defaultMain app =<< initState

app :: M.App State e ()
app =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return,
      M.appAttrMap = const attrMap
    }

-- TODO: show used letters
draw :: State -> [T.Widget n]
draw s =
  [ vBox
      [ str "Attempts:",
        drawAttempts (sAttempts s),
        drawAttempts futureAttempts,
        str "Next guess:",
        drawInput,
        status
      ]
  ]
  where
    status = str . sStatus $ s
    drawAttempts ats = vBox . map drawAttempt $ ats
    drawAttempt = hBox . map drawChar
    drawChar :: (Char, GuessType) -> T.Widget n
    drawChar c = charAttr (snd c) . border . padRight (T.Pad 1) . padLeft (T.Pad 1) . str . (: []) . fst $ c
    charAttr gt = case gt of
      NotInWord -> withAttr notInWordAttr
      WrongSpot -> withAttr wrongSpotAttr
      CorrectSpot -> withAttr correctSpotAttr
      Default -> id
    futureAttempts = map (const futureAttempt) [0 .. sMaxAttempts s - length (sAttempts s) - 1]
    futureAttempt = map (const (' ', Default)) [0 .. sWordSize s - 1]
    drawInput = drawAttempt . map (,Default) . (\cs -> cs ++ replicate (sWordSize s - length cs) ' ') . sInput $ s

handleEvent :: State -> T.BrickEvent n e -> T.EventM n (T.Next State)
handleEvent s e = case sGameStatus s of
  Ongoing -> case e of
    T.VtyEvent ve -> case ve of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt s
      V.EvKey (V.KChar 'l') [V.MCtrl] -> clearH
      V.EvKey (V.KChar 'r') [V.MCtrl] -> giveUpH
      V.EvKey (V.KChar k) [] -> inputH k
      V.EvKey V.KBS [] -> bspcH
      V.EvKey V.KEnter [] -> guessH
      _ -> M.continue s
    _ -> M.continue s
    where
      clearH = M.continue $ s {sInput = ""}
      giveUpH = M.continue $ s {sGameStatus = Loss, sStatus = printf "You lose, the word was \"%s\"" w}
      inputH k = M.continue $ s {sInput = take (sWordSize s) $ sInput s ++ [k]}
      bspcH = M.continue $ s {sInput = if null $ sInput s then "" else init $ sInput s}
      guessH = do
        let s' = s {sInput = ""}
            ns
              | length g /= length w = s' {sStatus = printf "Word size must be %d" $ length w}
              | not $ isCorrectWord g ws = s' {sStatus = printf "\"%s\" is not a valid word" g}
              | otherwise =
                case (g == w, length (sAttempts s'') == sMaxAttempts s'') of
                  (True, _) -> s'' {sGameStatus = Win, sStatus = "You guessed the word!"}
                  (_, True) -> s'' {sGameStatus = Loss, sStatus = printf "Wrong guess, the word was \"%s\"" w}
                  (_, False) -> s''
              where
                s'' = s' {sAttempts = sAttempts s ++ [attempt], sInput = ""}
        M.continue ns
  _ -> case e of
    T.VtyEvent ve -> case ve of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt s
      V.EvKey (V.KChar 'r') [] -> M.continue =<< liftIO initState
      _ -> M.continue s
    _ -> M.continue s
  where
    g = sInput s
    w = sWord s
    ws = sWords s
    attempt = guess g w

attrMap :: A.AttrMap
attrMap =
  A.attrMap
    V.defAttr
    [ (notInWordAttr, fg V.brightRed),
      (wrongSpotAttr, fg V.brightYellow),
      (correctSpotAttr, fg V.brightGreen)
    ]

notInWordAttr :: A.AttrName
notInWordAttr = "notInWordAttr"

wrongSpotAttr :: A.AttrName
wrongSpotAttr = "wrongSpotAttr"

correctSpotAttr :: A.AttrName
correctSpotAttr = "correctSpotAttr"
