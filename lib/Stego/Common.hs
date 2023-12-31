-- | Holds common types and functions for supporting the
-- audio steganography modules.
module Stego.Common
  ( EncodingType(..)
  , DecodingType(..)
  , StegoParams(..)
  , TotpPayload
  , TimestampPayload
  , DecodedPayload
  , DecodedFrame
  , Payload
  , Secret
  , calculateTotp
  , checkTotp
  , utcTimeToWord64
  , word64ToUtcTime
  , readBinWord64
  , readBinWord32
  , getEncodingType
  , shouldSkipFrame
  , getIsRealTime
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32)
import Data.OTP (HashAlgorithm(..), totp, totpCheck)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64, Word8)
import Numeric (readBin)

import Data.Audio.Wave (Frame)

-- | Supported encoding types
data EncodingType
  = LsbEncoding
  | EchoHideEncoding
  deriving (Show, Eq)

-- | Supported decoding types
data DecodingType
  = LsbDecoding
  | EchoHideDecoding
  deriving (Show, Eq)

-- | The secret key used to calculate TOTP result
type Secret = BS.ByteString

-- | Unsigned 32-bit representation of TOTP calculation result
type TotpPayload = Word32

-- | Unsigned 32-bit representation of UNIX timestamp argument
-- used in TOTP calculation
type TimestampPayload = Word32

-- | Optional payload to encode after verification headers
type Payload = Int32

-- | Tuple of time and totpPayload that is decoded from each frame
type DecodedPayload = (Word64, TotpPayload)

type DecodedFrame = (Int, [Int16], DecodedPayload)

-- | StegoParams instance used to capture parameters
data StegoParams =
  StegoParams Secret Word64 Word8 EncodingType Payload Bool
  deriving (Show, Eq)

-- | Extracts the EncodingType of provided StegoParams
getEncodingType :: StegoParams -> EncodingType
getEncodingType (StegoParams _ _ _ LsbEncoding _ _) = LsbEncoding
getEncodingType _ = EchoHideEncoding

-- | Extracts the boolean stegoParam indicating if the stego is real-time or not
getIsRealTime :: StegoParams -> Bool
getIsRealTime (StegoParams _ _ _ _ _ x) = x

-- | Calculates a TOTP value for the StegoParams at the provided UTCTime
calculateTotp :: StegoParams -> UTCTime -> TotpPayload
calculateTotp (StegoParams secret range numDigits _ _ _) time =
  totp SHA1 secret time range numDigits

-- | Checks if the provided TOTP value is valid for the given time and
-- stego params
checkTotp :: StegoParams -> UTCTime -> TotpPayload -> Bool
checkTotp (StegoParams secret range numDigits _ _ _) time =
  totpCheck SHA1 secret (0, 0) time range numDigits

-- | Converts a UTCTime into Word64 representation of POSIX seconds since epoch
utcTimeToWord64 :: UTCTime -> Word64
utcTimeToWord64 = round . utcTimeToPOSIXSeconds

-- | Converts Word64 representation of POSIX seconds since epoch into UTCTime
word64ToUtcTime :: Word64 -> UTCTime
word64ToUtcTime = posixSecondsToUTCTime . realToFrac

-- | Reads a provided binary representation into a Word64
readBinWord64 :: String -> Word64
readBinWord64 = fst . head . readBin

-- | Reads a provided binary representation into a Word32
readBinWord32 :: String -> Word32
readBinWord32 = fst . head . readBin

-- | Filtering function that determines if the provided frame is
-- suitable for encoding/decoding. This is mostly done by assessing if the
-- targeted bits are too quiet / low in energy.
shouldSkipFrame :: Frame -> Bool
shouldSkipFrame (_, f) =
  realToFrac (sum (map abs (take 128 f))) < (1E-3 :: Double)
