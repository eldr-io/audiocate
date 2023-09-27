{- |
- Encoding processor which allows for the enqueueing of Frames
- to be encoded and runs a dedicated thread for dequeueing and encoding frames
-}
module Stego.Encode.Encoder (
  newEncoder,
  enqueueFrame,
  startEncoder,
  takeStopVar,
)
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TMVar, TQueue, isEmptyTQueue, newEmptyTMVarIO, newTQueueIO, putTMVar, readTQueue, takeTMVar, writeTQueue)
import Control.Monad (void)
import Control.Monad.STM (atomically)
import Data.Audio.Wave (Frame, Frames)
import Data.Time (UTCTime, getCurrentTime)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (..), calculateTotp, getEncodingType, utcTimeToWord64)
import Stego.Encode.LSB (encodeFrame)

type FrameQ = TQueue Frame

data Encoder = Encoder
  { stegoParams :: StegoParams
  , frameQ :: FrameQ
  , finishedMutex :: TMVar Frames
  }

-- | Creates a new frame queue
newFrameQueue :: IO FrameQ
newFrameQueue = newTQueueIO

newFinishedMutex :: IO (TMVar Frames)
newFinishedMutex = newEmptyTMVarIO

newEncoder :: StegoParams -> IO Encoder
newEncoder stegoParams = do
  frameQ <- newFrameQueue
  finishedMutex <- newFinishedMutex
  let enc = Encoder stegoParams frameQ finishedMutex
  return enc

stopEncoder :: Encoder -> Frames -> IO ()
stopEncoder enc frames = atomically $ putTMVar (finishedMutex enc) frames

takeStopVar :: Encoder -> IO Frames
takeStopVar enc = atomically $ takeTMVar (finishedMutex enc)

dequeueFrame :: Encoder -> IO Frame
dequeueFrame enc = do
  frame <- atomically $ readTQueue (frameQ enc)
  putStrLn $ "Dequeueing frame:" <> show (length frame)
  threadDelay 500
  return frame

enqueueFrame :: Encoder -> Frame -> IO ()
enqueueFrame enc frame = do
  atomically $ writeTQueue (frameQ enc) frame

encodeFrame' :: StegoParams -> UTCTime -> Frame -> Frame
encodeFrame' stegoParams time frame
  | getEncodingType stegoParams == LsbEncoding =
      Stego.Encode.LSB.encodeFrame time64 totp frame
  | otherwise = undefined
 where
  time64 = utcTimeToWord64 time
  totp = calculateTotp stegoParams time

startEncoder :: Encoder -> IO ()
startEncoder encoder = do
  void $ forkIO $ proc encoder []
 where
  proc :: Encoder -> Frames -> IO ()
  proc enc frames = do
    frame <- dequeueFrame enc
    time <- getCurrentTime
    let encodedFrame = encodeFrame' (stegoParams enc) time frame
    let frs = frames ++ [encodedFrame]
    isEmpty <- atomically $ isEmptyTQueue (frameQ enc)
    putStrLn $ "Checking if is empty: " <> show isEmpty
    if isEmpty then stopEncoder enc frs else proc enc frs
