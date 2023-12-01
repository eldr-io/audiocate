module EncodeSpec
  ( spec
  ) where

import Control.Monad.Except (runExceptT)
import Data.Int (Int16)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Test.Hspec

import Command.EncodeCmd (doEncodeFramesWithEncoder)
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
  describe "Tests various encoding functionality" $
    -- setup test vars
   do
    let secret = encodeUtf8 (T.pack "test-secret")
    let timeValid :: Word64 = 5
    let stegoParams = StegoParams secret timeValid 6 LsbEncoding 0 False
    context "when passing it an empty frames list" $
      it "should return an empty encode result" $ do
        encoder <- newEncoder stegoParams
        result <- doEncodeFramesWithEncoder encoder []
        result `shouldBe` []
    context "when passing it frames that are below the cutoff threshold" $
      it "should return an encode result that skipped all frames" $ do
        let rawSamples :: [Int16] = [0 | i <- [0 .. 128]]
        let frame = (0, rawSamples)
        let frames = [frame]
        encoder <- newEncoder stegoParams
        result <- doEncodeFramesWithEncoder encoder frames
        result `shouldBe` [SkippedFrame frame]
    context
      "when passing it frames where the first 128 samples are below the cutoff threshold" $
      it "should return an encode result that skipped all frames" $
        -- the threshold algorithm looks at the first 128 samples of a frame
       do
        let rawSamples :: [Int16] = [0 | i <- [0 .. 128]]
        let rawSamplesRest :: [Int16] = [i * 50 | i <- [-20000 .. 20000]]
        let frame = (0, rawSamples ++ rawSamplesRest)
        let frames = [frame]
        encoder <- newEncoder stegoParams
        result <- doEncodeFramesWithEncoder encoder frames
        result `shouldBe` [SkippedFrame frame]
    context "when passing it frames from the sample1.wav testfile" $
      it "should return an encode result that verified 7 of the frames" $ do
        let inputFile = "test/corpus/sample1.wav"
        audio <- runExceptT (waveAudioFromFile inputFile)
        case audio of
          Left err -> err `shouldSatisfy` (not . null)
          Right wa -> do
            let frames = audioFrames wa
            encoder <- newEncoder stegoParams
            result <- doEncodeFramesWithEncoder encoder frames
            let (DRS total verified unverified skipped) = getResultStats result
            total `shouldBe` 18
            verified `shouldBe` 7
            unverified `shouldBe` 0
            skipped `shouldBe` 11
