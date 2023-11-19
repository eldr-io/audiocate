-- | Encoding processor which allows for the enqueueing of Frames
-- to be encoded and runs a dedicated thread for dequeueing and encoding frames
module Stego.Encode.Encoder
  ( newEncoder
  , newEncoderWithQC
  , enqueueFrame
  , stopEncoder
  , getResultChannel
  , Encoder(..)
  , EncoderResult(..)
  , EncoderResultChan
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TChan
  , TMVar
  , TQueue
  , dupTChan
  , newBroadcastTChanIO
  , newEmptyTMVarIO
  , newTQueueIO
  , putTMVar
  , readTQueue
  , writeTChan
  , writeTQueue
  )
import Control.Monad (void)
import Control.Monad.STM (atomically)
import Data.Time (UTCTime, getCurrentTime)

import Data.Audio.Wave (Frame)
import Stego.Common
  ( EncodingType(LsbEncoding)
  , StegoParams(..)
  , calculateTotp
  , getEncodingType
  , shouldSkipFrame
  , utcTimeToWord64
  )
import Stego.Encode.LSB (encodeFrame)

-- The operations supported by the Encoder
data EncoderOp
  = EncodeFrame Frame
  | StopEncoder (TMVar ())

-- Data Constructor for a result of applying an Encoder operation to a 
-- single frame
data EncoderResult
  = EncodedFrame Frame
  | SkippedFrame Frame
  | StoppingEncoder

type EncoderOpQ = TQueue EncoderOp

type EncoderResultChan = TChan EncoderResult

-- | The main data constructor for an Encoder instance
data Encoder =
  Encoder
    { stegoParams :: StegoParams
    , encoderOpQ :: EncoderOpQ
    , resultChan :: EncoderResultChan
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

-- | Creates a new encoder that can encode frames using
-- the provided StegoParams and uses the provided operation
-- queue and result channel
newEncoderWithQC :: StegoParams -> EncoderOpQ -> EncoderResultChan -> IO Encoder
newEncoderWithQC stegoParams opQ chan = do
  let enc = Encoder stegoParams opQ chan
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
enqueueFrame enc frame = do
  atomically $ writeTQueue (encoderOpQ enc) (EncodeFrame frame)

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

-- | The main run loop function for running an Encoder
runEncoder :: Encoder -> IO ()
runEncoder enc = loop
 where
  loop = do
    op <- atomically $ readTQueue (encoderOpQ enc)
    case op of
      (EncodeFrame (i, f)) -> do
        time <- getCurrentTime
        let shouldSkip = shouldSkipFrame (i, f)
        if shouldSkip
          then do
            atomically $ writeTChan (resultChan enc) (SkippedFrame (i, f))
            loop
          else do
            let encoded = encodeFrame' (stegoParams enc) time (i, f)
            atomically $ writeTChan (resultChan enc) (EncodedFrame encoded)
            loop
      (StopEncoder m) -> atomically $ do
        writeTChan (resultChan enc) StoppingEncoder
        putTMVar m ()
