module StegoSpec
  ( spec
  ) where
import Test.Hspec
import Stego.Common
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Audio.Wave (waveAudioFromFile, WaveAudio (bitSize))
import Control.Monad.Except (runExceptT)

spec :: Spec
spec =
  describe "Tests the StegoCommon module functionality" $ do
    context "when converting between Stego types" $ do
      it "tests the utcTimeToWord64 and word64ToUtcTime functions" $ do
        let t = utcTimeToWord64 $ posixSecondsToUTCTime 1.999999
        t `shouldBe` 2
        let t = utcTimeToWord64 $ posixSecondsToUTCTime 1.9999999
        t `shouldBe` 2
    context "when working on WAVE audio data" $ do
      it "can show the result of successfully loaded audio" $ do
        let inputFile = "test/corpus/sample1.wav"
        audio <- runExceptT (waveAudioFromFile inputFile)
        case audio of
          Left err -> length err `shouldBe` 0
          Right wa -> do
            length (show wa) `shouldBe` 147
            bitSize wa `shouldBe` 16
