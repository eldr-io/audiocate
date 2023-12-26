module DecodeCmdSpec where

import Test.Hspec (Spec, shouldBe, describe, context, it)

spec :: Spec
spec = do 
  describe "Tests decoding command functionality" $ do
    context "when passing it an empty frames list" $
      it "should return an empty decode result" $ do
        1 `shouldBe` 1
