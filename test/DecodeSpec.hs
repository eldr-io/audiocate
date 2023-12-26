module DecodeSpec where

import Command.DecodeCmd (doDecodeFramesWithDecoder)
import Data.Int (Int16)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Stego.Common (EncodingType(LsbEncoding, EchoHideEncoding), StegoParams(StegoParams))
import Stego.Decode.Decoder (newDecoder, DecoderResult (SkippedFrame), DecoderResultStats (DRS), getResultStats, getResultFrames)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Data.List (sort)

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
    context
      "when passing it frames where the first 128 samples are below the cutoff threshold" $
      it "should return an encode result that skipped all frames" $
        -- the threshold algorithm looks at the first 128 samples of a frame
       do
        let rawSamples :: [Int16] = [0 | i <- [0 .. 128]]
        let rawSamplesRest :: [Int16] = [i * 50 | i <- [-20000 .. 20000]]
        let frame = (0, rawSamples ++ rawSamplesRest)
        let frames = [frame]
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder frames
        result `shouldBe` [SkippedFrame frame]
    context
      "when passing it frames that have less than 128 samples" $
      it "should return an encode result that skipped all frames" $ do
        let rawSamples :: [Int16] = [i + 1 | i <- [0 .. 118]]
        let frame = (0, rawSamples)
        let frames = [frame]
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder frames
        result `shouldBe` [SkippedFrame frame]
    context "when passing it frames that are valid for decoding but not encoded" $
      it "should return a decode result that did not verify any frames" $ do
        let rawSamplesRest :: [Int16] = [i + 1 * 50 | i <- [0 .. 20000]]
        let frame = (0, rawSamplesRest)
        let frames = [frame, frame, frame, frame, frame]
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder frames
        let (DRS total verified unverified skipped) = getResultStats result
        total `shouldBe` 5
        verified `shouldBe` 0
        unverified `shouldBe` 5
        skipped `shouldBe` 0
    context "when passing it frames that are valid for decoding unordered" $
      it "should return a decode result that did not verify any frames" $ do
        let rawSamplesRest :: [Int16] = [i + 1 * 50 | i <- [0 .. 20000]]
        let frame = (0, rawSamplesRest)
        let frame1 = (3, rawSamplesRest)
        let frame2 = (2, rawSamplesRest)
        let frames = [frame1, frame, frame2, frame2, frame1]
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder frames
        let (DRS total verified unverified skipped) = getResultStats (sort result)
        total `shouldBe` 5
        verified `shouldBe` 0
        unverified `shouldBe` 5
        skipped `shouldBe` 0
    context "when passing it frames that are valid for decoding unordered" $
      it "should return a showable decode stats result that did not verify any frames" $ do
        let rawSamplesRest :: [Int16] = [i + 1 * 50 | i <- [0 .. 20000]]
        let frame = (0, rawSamplesRest)
        let frame1 = (3, rawSamplesRest)
        let frame2 = (2, rawSamplesRest)
        let frames = [frame1, frame, frame2, frame2, frame1]
        decoder <- newDecoder stegoParams
        result <- doDecodeFramesWithDecoder decoder frames
        let stats = getResultStats (sort result)
        print stats
        let resultFrames = getResultFrames result
        length resultFrames `shouldBe` 5
        length (show stats) `shouldBe` 132
    context "when passing it frames that are valid for decoding but with an unimplemented encoding type" $
      it "should return a decode result that skipped all frames" $ do
        let stegoParams' = StegoParams secret timeValid 6 EchoHideEncoding 0 False
        let rawSamplesRest :: [Int16] = [i + 1 * 50 | i <- [0 .. 20000]]
        let frame = (0, rawSamplesRest)
        let frames = [frame, frame, frame, frame, frame]
        decoder <- newDecoder stegoParams'
        result <- doDecodeFramesWithDecoder decoder frames
        let (DRS total verified unverified skipped) = getResultStats result
        total `shouldBe` 5
        verified `shouldBe` 0
        unverified `shouldBe` 0
        skipped `shouldBe` 5
