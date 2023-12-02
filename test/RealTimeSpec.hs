module RealTimeSpec 
  ( spec
  ) where
import Test.Hspec ( describe, it, shouldBe, Spec, context, shouldNotBe )
import Audiocate (Command(Encode, Decode), CommandReturnCode (CmdSuccess))
import Command.Cmd (interpretCmd)
import Control.Concurrent (threadDelay)

spec :: Spec
spec =
  describe "Tests encoding and decoding with the real-time flag set" $ do
     context "when passing it an encode command targeting the sample1.wav test file" $
        it "should return an encode result that encoded the expected 7 frames" $ do
          let inputFile = "test/corpus/sample1.wav"
          let outputFile = "test/output/sample1_rt_out.wav"
          let encodeCmd = Encode "test-secret" 2 inputFile outputFile
          result <- interpretCmd encodeCmd True
          result `shouldBe` CmdSuccess
          threadDelay 5000000
          let decodeCmd = Decode "test-secret" 2 outputFile
          result <- interpretCmd decodeCmd True
          result `shouldNotBe` CmdSuccess
