-- | Holds common types and functions for supporting the
--   audio steganography modules.
module Stego.Common
  ( EncodingType (..),
    DecodingType (..),
    StegoParams (..),
    bitInitPattern,
    TotpPayload,
    TimestampPayload,
    Payload,
    Secret,
  )
where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Word (Word32, Word8)

data EncodingType = LsbEncoding | EchoHideEncoding

data DecodingType = LsbDecoding | EchoHideDecoding

-- | Used to identify the start of an embedded payload
bitInitPattern :: Word8
bitInitPattern = 0b11001110

-- | The secret key used to calculate TOTP result
type Secret = BS.ByteString

-- | Unsigned 32-bit representation of TOTP calculation result
type TotpPayload = Word32

-- | Unsigned 32-bit representation of UNIX timestamp argument
-- used in TOTP calculation
type TimestampPayload = Word32

-- | Optional payload to encode after verification headers
type Payload = Int32

-- | StegoParams instance used to capture parameters
data StegoParams = StegoParams Secret EncodingType Payload
