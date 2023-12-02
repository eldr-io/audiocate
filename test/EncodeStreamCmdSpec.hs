module EncodeStreamCmdSpec
  ( spec
  ) where

import Control.Monad.Except (runExceptT)
import Data.Int (Int16)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldSatisfy, shouldNotBe)

import Audiocate (Command(EncodeStream, DecodeStream), CommandReturnCode(CmdSuccess))
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
  describe "Tests the encoding stream command functionality" $ do
    context
      "when passing it an encode stream command targeting the sample1.wav test file" $
      it "should return an encode result that encoded the expected 7 frames" $ do
        let inputFile = "test/corpus/sample1.wav"
        let outputFile = "test/output/sample1_stream_out.wav"
        let encodeCmd = EncodeStream "test-secret" 5 inputFile outputFile
        result <- interpretCmd encodeCmd False
        result `shouldBe` CmdSuccess
        let decodeCmd = DecodeStream "test-secret" 5 outputFile
        decode_result <- interpretCmd decodeCmd False
        decode_result `shouldBe` CmdSuccess
