module DecodeSpec where
import Test.Hspec (Spec, shouldBe, describe, context, it)
import Data.Word (Word64)
import Stego.Common (StegoParams(StegoParams), EncodingType (LsbEncoding))
import Stego.Decode.Decoder (newDecoder)
import Command.DecodeCmd (doDecodeFramesWithDecoder)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Tests decoding functionality" $ do
    let secret = encodeUtf8 (T.pack "test-secret")
    let timeValid :: Word64 = 5
    let stegoParams = StegoParams secret timeValid 6 LsbEncoding 0 False
    context "when passing it an empty frames list" $
      it "should return an empty decode result" $ do
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder []
        result `shouldBe` []
