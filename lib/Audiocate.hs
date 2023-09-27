module Audiocate (someFunc) where

import Data.Audio.Wave ()
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (StegoParams), calculateTotp, checkTotp)

someFunc :: IO ()
someFunc = do
  time <- getCurrentTime
  let stegoParams = StegoParams (encodeUtf8 (T.pack "my secret")) 30 6 LsbEncoding 123
  let totpValue = calculateTotp stegoParams time
  print totpValue
  print $ checkTotp stegoParams time totpValue
