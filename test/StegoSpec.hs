module StegoSpec
  ( spec
  ) where
import Test.Hspec
import Stego.Common
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

spec :: Spec
spec =
  describe "Tests the StegoCommon module functionality" $
    -- setup test vars
   do
    it "tests the utcTimeToWord64 and word64ToUtcTime functions" $ do
      let t = utcTimeToWord64 $ posixSecondsToUTCTime 1.999999
      t `shouldBe` 2
      let t = utcTimeToWord64 $ posixSecondsToUTCTime 1.9999999
      t `shouldBe` 2
