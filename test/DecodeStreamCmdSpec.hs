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
      "when passing it an decode stream command targeting the sample2_stream_in.wav test file" $
      it "should return a result that decoded all of the encoded frames" $ do
        let inputFile = "test/corpus/sample2_stream_in.wav"
        let encodeCmd = DecodeStream "test-secret" 5 inputFile
        result <- interpretCmd encodeCmd
        result `shouldBe` CmdSuccess
