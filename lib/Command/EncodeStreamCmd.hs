module Command.EncodeStreamCmd
  ( runEncodeStreamCmd
  ) where

import Data.Audio.Stream (sinkSnd, sourceSnd)
import Data.Conduit.Audio (mapSamples, Duration (Frames), AudioSource (AudioSource, frames, channels, source), framesToSeconds, vectorFrames)
import Stego.Common (StegoParams, utcTimeToWord64, calculateTotp)
import Control.Monad.Trans.Resource (runResourceT)
import Foreign.Storable (Storable)
import Data.Int (Int16)
import Data.Time (getCurrentTime)
import Stego.Encode.Encoder (newEncoder)
import Stego.Decode.Decoder (getResultChannel, enqueueFrame)
import Control.Concurrent (forkIO)
import Stego.Encode.LSB (encodeFrame)
import Data.Audio.Wave (WaveAudio(rate))
import Conduit (MonadIO(liftIO))

runEncodeStreamCmd :: StegoParams -> FilePath -> FilePath -> IO ()
runEncodeStreamCmd stegoP inputFile outputFile = do
  (input :: AudioSource m Int16, fmt) <- sourceSnd inputFile
  print (frames input)
  runResourceT $ do 
    time <- liftIO $ getCurrentTime
    liftIO $ print time
    -- print (vectorFrames (source input) (channels input))
    -- let time64 = utcTimeToWord64 time
    --     totp = calculateTotp stegoP time
    sinkSnd outputFile fmt input
