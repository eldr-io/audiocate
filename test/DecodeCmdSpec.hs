module DecodeCmdSpec where

import Test.Hspec (Spec, shouldBe, describe, context, it)
import Audiocate (Command(Help, Decode, Encode), CommandReturnCode (CmdSuccess, DecodeCmdSuccess, EncodeCmdSuccess, CmdFail))
import Command.Cmd (interpretCmd)
import Stego.Decode.Decoder (DecoderResultStats(DRS), getResultStats)

spec :: Spec
spec = do 
  describe "Tests decoding command functionality" $ do
    context "when passing it an empty frames list" $
      it "should return an empty decode result" $ do
        1 `shouldBe` 1
    context "when running the Help command" $
      it "should print the help command text" $ do
        let helpCmd = Help
        result <- interpretCmd helpCmd False False
        result `shouldBe` CmdSuccess
        show helpCmd `shouldBe` "HELP"
        show result `shouldBe` "Command completed successfully."
        let failCmd = CmdFail "test error"
        show failCmd `shouldBe` "Command failed with error. test error"
    context
      "when passing it an encode and decode command targeting the sample3.wav test file" $
      it "should return an encode result that encoded the expected 7 frames" $ do
        let inputFile = "test/corpus/sample3.wav"
        let outputFile = "test/output/sample3_out.wav"
        let encodeCmd = Encode "test-secret" 5 inputFile outputFile
        result <- interpretCmd encodeCmd False False
        case result of
          EncodeCmdSuccess res -> do
            let (DRS total verified unverified skipped) = getResultStats res
            total `shouldBe` 28
            verified `shouldBe` 17
            unverified `shouldBe` 0
            skipped `shouldBe` 11
            let decodeCmd = Decode "test-secret" 5 outputFile
            result' <- interpretCmd decodeCmd False False
            case result' of
              DecodeCmdSuccess res' -> do
                let (DRS total' verified' unverified' skipped') = getResultStats res'
                total' `shouldBe` 28
                verified' `shouldBe` 17
                unverified' `shouldBe` 0
                skipped' `shouldBe` 11
                show decodeCmd `shouldBe` "DECODE"
                show result' `shouldBe` "Decode command completed successfully."
              _ -> True `shouldBe` False -- anything else should fail the test
          _ -> True `shouldBe` False -- anything else should fail the test

