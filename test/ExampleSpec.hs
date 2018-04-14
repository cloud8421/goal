module ExampleSpec (spec) where

import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "project test suite" $ do
    it "executes successfully" $ do
      head [23 ..] `shouldBe` (23 :: Int)
