module EncodeSpec (
  spec
  )
where

import Test.Hspec 
import Stego.Encode.Encoder (Encoder(Encoder, stegoParams), newEncoder)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)

import Command.EncodeCmd (doEncodeFramesWithEncoder)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (..))

spec :: Spec
spec = describe "Tests various encoding functionality" $ do
  context "when passing it an empty frames list" $
    it "should return an empty encode result" $ do
      let s = encodeUtf8 (T.pack "test-secret")
      let t :: Word64 = 5
      let stegoParams = StegoParams s t 6 LsbEncoding 123
      encoder <- newEncoder stegoParams
      result <- doEncodeFramesWithEncoder encoder [] 
      result `shouldBe` []
      
