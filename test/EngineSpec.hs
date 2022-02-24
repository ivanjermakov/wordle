module EngineSpec where

import Data.Time
import Engine
import Test.Hspec

spec :: Spec
spec = do
  describe "guess" $ do
    let test g t gts =
          shouldBe
            (map snd $ guess g t)
            gts
    it "abc -> abc" $ do
      test "abc" "abc" [CorrectSpot, CorrectSpot, CorrectSpot]
    it "aac -> abc" $ do
      test "aac" "abc" [CorrectSpot, NotInWord, CorrectSpot]
    it "abcb -> abcc" $ do
      test "abcd" "abcc" [CorrectSpot, CorrectSpot, CorrectSpot, NotInWord]
    it "correct chars" $ do
      map fst (guess "allow" "skill") `shouldBe` "allow"
    it "fever -> skill" $ do
      test "fever" "skill" [NotInWord, NotInWord, NotInWord, NotInWord, NotInWord]
    it "allow -> skill" $ do
      test "allow" "skill" [NotInWord, WrongSpot, WrongSpot, NotInWord, NotInWord]
    it "swiss -> skill" $ do
      test "swiss" "skill" [CorrectSpot, NotInWord, CorrectSpot, NotInWord, NotInWord]
    it "issue -> skill" $ do
      test "issue" "skill" [WrongSpot, WrongSpot, NotInWord, NotInWord, NotInWord]
    it "skill -> skill" $ do
      test "skill" "skill" [CorrectSpot, CorrectSpot, CorrectSpot, CorrectSpot, CorrectSpot]
    it "reels -> myers" $ do
      test "reels" "myers" [WrongSpot, NotInWord, CorrectSpot, NotInWord, CorrectSpot]
    it "seers -> myers" $ do
      test "reels" "myers" [WrongSpot, NotInWord, CorrectSpot, NotInWord, CorrectSpot]
    it "train -> timer" $ do
      test "train" "timer" [CorrectSpot, WrongSpot, NotInWord, WrongSpot, NotInWord]
    it "trine -> timer" $ do
      test "trine" "timer" [CorrectSpot, WrongSpot, WrongSpot, NotInWord, WrongSpot]
    it "their -> timer" $ do
      test "their" "timer" [CorrectSpot, NotInWord, WrongSpot, WrongSpot, CorrectSpot]
  describe "dailyWordIndex" $ do
    let test timeStr idx = do
          act <- dailyWordIndex $ parseTimeOrError True defaultTimeLocale "%FT%T" timeStr
          shouldBe act idx
    it "2022-01-01T00:00:00" $ do
      test "2022-01-01T00:00:00" 195
      test "2022-01-01T00:01:00" 195
      test "2022-01-01T23:59:59" 195
    it "2022-01-02T00:00:00" $ do
      test "2022-01-02T00:00:00" 196
      test "2022-01-02T00:01:00" 196
      test "2022-01-02T23:59:59" 196
    it "2022-01-02T00:00:00" $ do
      test "2022-02-10T00:00:00" 235
  describe "pickDailyWord" $ do
    let test timeStr w = do
          dict <- words <$> readFile "resource/dict/official.txt"
          let day = parseTimeOrError True defaultTimeLocale "%FT%T" timeStr
          word <- pickDailyWord day dict
          shouldBe word w
    it "2022-01-01T00:00:00" $ do
      test "2022-01-01T00:00:00" "unify"
      test "2022-01-01T00:01:00" "unify"
      test "2022-01-01T23:59:59" "unify"
    it "2022-01-02T00:00:00" $ do
      test "2022-01-02T00:00:00" "rebus"
      test "2022-01-02T00:01:00" "rebus"
      test "2022-01-02T23:59:59" "rebus"
    it "2022-01-02T00:00:00" $ do
      test "2022-02-10T00:00:00" "humor"
