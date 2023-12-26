module DecodeSpec where

import Command.DecodeCmd (doDecodeFramesWithDecoder)
import Data.Int (Int16)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Stego.Common (EncodingType(LsbEncoding), StegoParams(StegoParams))
import Stego.Decode.Decoder (newDecoder, DecoderResult (SkippedFrame))
import Test.Hspec (Spec, context, describe, it, shouldBe)

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
    context "when passing it frames that are below the cutoff threshold" $
      it "should return an encode result that skipped all frames" $ do
        let rawSamples :: [Int16] = [0 | i <- [0 .. 128]]
        let frame = (0, rawSamples)
        let frames = [frame]
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder frames
        result `shouldBe` [SkippedFrame frame]
