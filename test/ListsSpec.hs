module ListsSpec (spec) where

import Test.Hspec
import Lists
import Control.Exception

spec :: Spec
spec = do
  describe "myLast" $ do
    it "returns the ultimate element of a list" $ do
      myLast ['1']`shouldBe` '1'
      myLast [1] `shouldBe` 1
      myLast "dcba" `shouldBe` 'a'

    context "when supplied with an empty list" $ do
      it "throws an exception" $ do
        myLast []`shouldThrow` anyException

  describe "myButLast" $ do
    it "returns the penultimate element of a list" $ do
      myButLast [1,2] `shouldBe` 1
      myButLast [1,2,3] `shouldBe` 2
      myButLast "allo" `shouldBe` 'l'

    context "when supplied with an empty list" $ do
      it "throws an exception" $ do
        myButLast [] `shouldThrow` anyException

    context "when supplied with a singleton list" $ do
      it "throws an exception" $ do
        evaluate (myButLast [1]) `shouldThrow` anyException

  describe "elementAt" $ do
    it "returns th kth element of a list" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
      error "asd" `shouldThrow` anyException

  describe "myLength" $ do
    it "returns the lenght of a list" $ do
      myLength [] `shouldBe` 0
      myLength [1] `shouldBe` 1
      myLength [1,2] `shouldBe` 2

  describe "myReverse" $ do
    it "returns an inverted list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]
