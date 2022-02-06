module EngineSpec where

import Engine
import Test.Hspec

spec :: Spec
spec = do
  describe "guess" $ do
    it "double of double in wrong spot" $ do
      shouldBe
        (guess "allow" "skill")
        [('a', NotInWord), ('l', WrongSpot), ('l', WrongSpot), ('o', NotInWord), ('w', NotInWord)]
    it "triple of single in correct spot" $ do
      shouldBe
        (guess "swiss" "skill")
        [('s', CorrectSpot), ('w', NotInWord), ('i', CorrectSpot), ('s', NotInWord), ('s', NotInWord)]
    it "double of single in wrong spot" $ do
      shouldBe
        (guess "issue" "skill")
        [('i', WrongSpot), ('s', WrongSpot), ('s', NotInWord), ('u', NotInWord), ('e', NotInWord)]
    it "correct word" $ do
      shouldBe
        (guess "skill" "skill")
        [('s', CorrectSpot), ('k', CorrectSpot), ('i', CorrectSpot), ('l', CorrectSpot), ('l', CorrectSpot)]
