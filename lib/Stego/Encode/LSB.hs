{- | Module for encoding and decoding into Audio samples using
- the Least Significant Bit (LSB) approach. Payloads are embedded
- into the 16th LSB of each 16-bit sample provided.
-}
module Stego.Encode.LSB (
  encodeFrame,
  decodeFrame,
)
where

import Bits.Show (showFiniteBits)
import Data.Audio.Wave
import Data.Bits
import Data.Word (Word64)
import Stego.Common (TotpPayload)

{- | Encodes the provided frame by embedding the timestamp and
TotpPayload into the start of the frame
-}
encodeFrame :: Word64 -> TotpPayload -> Frame -> Frame
encodeFrame time payload frame = encTime ++ enc ++ drop 96 frame
 where
  encodeBits = map (\(x, y) -> if y then x `setBit` 0 else x `clearBit` 0)
  enc = encodeBits $ zip (drop 64 frame) (zs 31 payload)
  encTime = encodeBits $ zip frame (zs 63 time)
  zs (-1) _ = []
  zs n p = (p .&. bit n /= 0) : zs (n - 1) p

{- | Decodes the provided frame by extracting the various payload
components
-}
decodeFrame :: Frame -> (String, String)
decodeFrame f =
  let timePayload = map (last . showFiniteBits) (take 64 f)
      totpPayload = map (last . showFiniteBits) (take 32 (drop 64 f))
   in (timePayload, totpPayload)
