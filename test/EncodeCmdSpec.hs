module EncodeCmdSpec
  ( spec
  ) where

import Control.Monad.Except (runExceptT)
import Data.Int (Int16)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldSatisfy, shouldNotBe)

import Audiocate
  ( Command(Encode)
  , CommandReturnCode(CmdSuccess, EncodeCmdSuccess)
  )
import Command.Cmd (interpretCmd)
import Command.EncodeCmd (doEncodeFramesWithEncoder, runEncodeCmd)
import Data.Audio.Wave
  ( Frames
  , WaveAudio(..)
  , waveAudioFromFile
  , waveAudioToFile
  )
import Stego.Common (EncodingType(LsbEncoding), StegoParams(..))
import Stego.Decode.Decoder
  ( DecoderResult(SkippedFrame)
  , DecoderResultStats(DRS)
  , getResultStats
  )
import Stego.Encode.Encoder (Encoder(Encoder, stegoParams), newEncoder)

spec :: Spec
spec =
  describe "the encoding command" $ do
    context
      "when passing it an encode command targeting the sample1.wav test file" $
      it "should return an encode result that verified the expected 7 frames" $ do
        let inputFile = "test/corpus/sample1.wav"
        let outputFile = "test/output/sample1_out.wav"
        let encodeCmd = Encode "test-secret" 5 inputFile outputFile
        result <- interpretCmd encodeCmd False False
        case result of
          EncodeCmdSuccess res -> do
            let (DRS total verified unverified skipped) = getResultStats res
            total `shouldBe` 18
            verified `shouldBe` 7
            unverified `shouldBe` 0
            skipped `shouldBe` 11
            show encodeCmd `shouldBe` "ENCODE"
            show result `shouldBe` "Encode command completed successfully."
          _ -> True `shouldBe` False -- anything else should fail the test
    context
      "when passing it an encode command targeting the sample2.wav test file" $
      it "should return an encode result that verified the expected 112 frames" $ do
        let inputFile = "test/corpus/sample2.wav"
        let outputFile = "test/output/sample2_out.wav"
        let encodeCmd = Encode "test-secret" 5 inputFile outputFile
        result <- interpretCmd encodeCmd False False
        case result of
          EncodeCmdSuccess res -> do
            let (DRS total verified unverified skipped) = getResultStats res
            total `shouldBe` 189
            verified `shouldBe` 112
            unverified `shouldBe` 0
            skipped `shouldBe` 77
          _ -> True `shouldBe` False -- anything else should fail the test
