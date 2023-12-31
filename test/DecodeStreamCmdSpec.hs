module DecodeStreamCmdSpec
  ( spec
  ) where

import Control.Monad.Except (runExceptT)
import Data.Int (Int16)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldNotBe)

import Audiocate (Command(Decode, DecodeStream), CommandReturnCode(CmdSuccess))
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
  describe "Tests the decoding stream command functionality" $ do
    context
      "when passing it an decode stream command targeting the sample2_stream_in.wav test file" $ do
      it "should return a result that decoded all of the encoded frames" $ do
        let inputFile = "test/corpus/sample3_stream_in.wav"
        let testSecret = "125@#1!12asde!3214[12345%¤¤21qassdf==:?213!3324124]"
        let cmd = DecodeStream testSecret 5 inputFile
        result <- interpretCmd cmd False False
        result `shouldBe` CmdSuccess
        show cmd `shouldBe` "DECODESTREAM"
        show result `shouldBe` "Command completed successfully."
      it
        "should return a result that decoded all of the encoded frames verbosely" $ do
        let inputFile = "test/corpus/sample3_stream_in.wav"
        let testSecret = "125@#1!12asde!3214[12345%¤¤21qassdf==:?213!3324124]"
        let cmd = DecodeStream testSecret 5 inputFile
        result <- interpretCmd cmd False True
        result `shouldBe` CmdSuccess
      it "should return a success result that decoded frames in realtime" $ do
        let inputFile = "test/corpus/sample2_stream_in.wav"
        let cmd = DecodeStream "test-secret" 5 inputFile
        result <- interpretCmd cmd True False
        result `shouldBe` CmdSuccess
