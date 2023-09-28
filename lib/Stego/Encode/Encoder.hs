-- |
-- - Encoding processor which allows for the enqueueing of Frames
-- - to be encoded and runs a dedicated thread for dequeueing and encoding frames
module Stego.Encode.Encoder
  ( newEncoder,
    enqueueFrame,
    stopEncoder,
    getResultChannel,
    EncoderResult (..),
    EncoderResultChan,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( TChan,
    TMVar,
    TQueue,
    dupTChan,
    newBroadcastTChanIO,
    newEmptyTMVarIO,
    newTQueueIO,
    putTMVar,
    readTQueue,
    writeTChan,
    writeTQueue,
  )
import Control.Monad (void)
import Control.Monad.STM (atomically)
import Data.Audio.Wave (Frame)
import Data.Time (UTCTime, getCurrentTime)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (..), calculateTotp, getEncodingType, utcTimeToWord64)
import Stego.Encode.LSB (encodeFrame)

data EncoderOp = EncodeFrame Frame | StopEncoder (TMVar ())

data EncoderResult = EncodedFrame Frame | SkippedFrame Frame | StoppingEncoder

type EncoderOpQ = TQueue EncoderOp

type EncoderResultChan = TChan EncoderResult

data Encoder = Encoder
  { stegoParams :: StegoParams,
    encoderOpQ :: EncoderOpQ,
    resultChan :: EncoderResultChan
  }

-- | Creates a new encoder that can encode frames using
-- the provided StegoParams
newEncoder :: StegoParams -> IO Encoder
newEncoder stegoParams = do
  frameQ <- newTQueueIO
  encFrameChan <- newBroadcastTChanIO
  let enc = Encoder stegoParams frameQ encFrameChan
  void $ forkIO $ runEncoder enc
  return enc

-- | Returns a duplicate of the Broadcast result channel used 
-- by the encoder.
getResultChannel :: Encoder -> IO (TChan EncoderResult)
getResultChannel = atomically . dupTChan . resultChan

-- | Sets the stop signal TMVar for the encoder to break out
-- it's encoding loop.
stopEncoder :: Encoder -> IO (TMVar ())
stopEncoder enc = do
  m <- newEmptyTMVarIO
  atomically $ writeTQueue (encoderOpQ enc) (StopEncoder m)
  pure m

-- | Enqueues a frame for the encoder to encode
enqueueFrame :: Encoder -> Frame -> IO ()
enqueueFrame enc frame = atomically $ writeTQueue (encoderOpQ enc) (EncodeFrame frame)

-- | Used internally by the encoder to call on the appropriate encoding
-- technique as dictated by the stegoParams
encodeFrame' :: StegoParams -> UTCTime -> Frame -> Frame
encodeFrame' stegoParams time frame
  | getEncodingType stegoParams == LsbEncoding =
      Stego.Encode.LSB.encodeFrame time64 totp frame
  | otherwise = undefined
  where
    time64 = utcTimeToWord64 time
    totp = calculateTotp stegoParams time

runEncoder :: Encoder -> IO ()
runEncoder enc = loop
  where
    loop = do
      op <- atomically $ readTQueue (encoderOpQ enc)
      case op of
        (EncodeFrame f) -> do
          time <- getCurrentTime
          let shouldSkip = realToFrac (sum (map abs (take 128 f))) < (1E-3 :: Double)
          if shouldSkip then do
            atomically $ writeTChan (resultChan enc) (SkippedFrame (replicate (length f) 0))
            loop
          else do
            let encoded = encodeFrame' (stegoParams enc) time f
            atomically $ writeTChan (resultChan enc) (EncodedFrame encoded)
            loop
        (StopEncoder m) -> atomically $ do
          writeTChan (resultChan enc) StoppingEncoder
          putTMVar m ()
