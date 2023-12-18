module RealTimeSpec 
  ( spec
  ) where
import Test.Hspec ( describe, it, shouldBe, Spec, context, shouldNotBe )
import Audiocate (Command(Encode, Decode), CommandReturnCode (CmdSuccess, EncodeCmdSuccess, DecodeCmdSuccess))
import Command.Cmd (interpretCmd)
import Control.Concurrent (threadDelay)
import Stego.Decode.Decoder (DecoderResultStats(DRS), getResultStats)

spec :: Spec
spec =
  describe "encoding and decoding with the real-time flag set" $ do
     context "when passing it an encode command targeting the sample1.wav test file" $
        it "should successfully decode the first time within the time window, and then fail the second time" $ do
          let inputFile = "test/corpus/sample1.wav"
          let outputFile = "test/output/sample1_rt_out.wav"
          let secret = "21ø!2312422mmsfiuetest#@@1@sasf//"
          let encodeCmd = Encode secret 5 inputFile outputFile
          result <- interpretCmd encodeCmd True
          case result of
            EncodeCmdSuccess res -> do
              let (DRS total verified unverified skipped) = getResultStats res
              total `shouldBe` 18
              verified `shouldBe` 7
              unverified `shouldBe` 0
              skipped `shouldBe` 11
            _ -> True `shouldBe` False -- fail the test
          
          -- first pass at decode within time frame
          let decodeCmd = Decode secret 5 outputFile
          result <- interpretCmd decodeCmd True
          case result of
            DecodeCmdSuccess res -> do
              let (DRS total verified unverified skipped) = getResultStats res
              total `shouldBe` 18
              verified `shouldBe` 7
              unverified `shouldBe` 0
              skipped `shouldBe` 11
            _ -> True `shouldBe` False -- fail the test

          -- wait long enough for the time window to close
          threadDelay 7000000
          -- This is after the time window has closed, so we should have no verified 
          -- chunks in the decode
          let decodeCmd = Decode secret 5 outputFile
          result <- interpretCmd decodeCmd True
          case result of
            DecodeCmdSuccess res -> do
              let (DRS total verified unverified skipped) = getResultStats res
              total `shouldBe` 18
              verified `shouldBe` 0
              unverified `shouldBe` 7
              skipped `shouldBe` 11
            _ -> True `shouldBe` False -- fail the test
