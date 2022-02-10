module EngineSpec where

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
