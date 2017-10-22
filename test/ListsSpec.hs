module ListsSpec (spec) where

import Test.Hspec
import Lists

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

    context "when sipplied with an empty list" $ do
      it "throws an exception" $ do
        myButLast [] `shouldThrow` anyException


