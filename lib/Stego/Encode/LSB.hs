{- | Module for encoding into Audio samples using
- the Least Significant Bit (LSB) approach. Payloads are embedded
- into the 16th LSB of each 16-bit sample provided.
-}
module Stego.Encode.LSB (
  encodeFrame,
)
where

import Data.Audio.Wave
import Data.Bits
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Stego.Common (TotpPayload)

{- | Encodes the provided frame by embedding the timestamp and
TotpPayload into the start of the frame
-}
encodeFrame :: Word64 -> TotpPayload -> Frame -> Frame
encodeFrame time payload (i, frame) = (i, encTime ++ enc ++ drop 96 frame)
 where
  tSize = fromJust $ bitSizeMaybe time
  pSize = fromJust $ bitSizeMaybe payload
  encodeBits = map (\(x, y) -> if y then x `setBit` 0 else x `clearBit` 0)
  enc = encodeBits $ zip (drop tSize frame) (zs (pSize - 1) payload)
  encTime = encodeBits $ zip frame (zs (tSize - 1) time)
  zs (-1) _ = []
  zs n p = (p .&. bit n /= 0) : zs (n - 1) p
