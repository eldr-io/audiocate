{- | Module for decoding Audio samples using
- the Least Significant Bit (LSB) approach. Payloads are decoded
- from the 16th LSB of each 16-bit sample provided.
-}
module Stego.Decode.LSB (
  decodeFrame,
)
where

import Bits.Show (showFiniteBits)
import Data.Audio.Wave (Frame)

{- | Decodes the provided frame by extracting the various payload
components
-}
decodeFrame :: Frame -> (Int, String, String)
decodeFrame (i,f) =
  let timePayload = map (last . showFiniteBits) (take 64 f)
      totpPayload = map (last . showFiniteBits) (take 32 (drop 64 f))
   in (i, timePayload, totpPayload)
