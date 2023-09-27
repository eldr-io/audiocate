{- |
- Decoding processor which allows for the enqueueing of Frames
- to be decoded and runs a dedicated thread for dequeueing and decoding frames
-}
module Stego.Decode.Decoder ( Decoder (..)) where

import Control.Concurrent.STM (TMVar, TQueue)
import Data.Audio.Wave (Frame)
import Stego.Common (DecodedFrame, StegoParams (..))

type FrameQ = TQueue Frame

data Decoder = Decoder
  { stegoParams :: StegoParams
  , frameQ :: FrameQ
  , finishedMutex :: TMVar [DecodedFrame]
  }
