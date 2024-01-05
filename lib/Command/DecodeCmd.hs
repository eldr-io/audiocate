module Command.DecodeCmd
  ( runDecodeCmd
  , doDecodeFramesWithDecoder
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , putTMVar
  , readTChan
  , takeTMVar
  )
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.List (sort)
import Data.Time (diffUTCTime, getCurrentTime)

import Data.Audio.Wave (Frames, WaveAudio(..), waveAudioFromFile)
import Stego.Common (StegoParams(..))
import Stego.Decode.Decoder
  ( Decoder
  , DecoderResult(StoppingDecoder)
  , DecoderResultList
  , enqueueFrame
  , getResultChannel
  , getResultStats
  , newDecoder
  , stopDecoder
  )

-- | Creates a decoder instance using doDecodeWaveAudio to decode the audio read from 
-- the input audiofile. Returns an error or the DecoderResultList of the decoding.
runDecodeCmd :: StegoParams -> FilePath -> IO (Either String DecoderResultList)
runDecodeCmd stegoParams inputFile = do
  startTime <- getCurrentTime
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> do
      pure (Left err)
    Right wa -> do
      result <- doDecodeWaveAudio stegoParams wa
      putStrLn "\nDecode Result "
      print $ getResultStats result
      endTime <- getCurrentTime
      putStrLn $ "Completed decode in " <> show (diffUTCTime endTime startTime)
      pure (Right result)

-- | Creates a decoder instance to decode the audio in the provided WaveAudio
-- instance. Returns the resulting DecoderResultList. 
doDecodeWaveAudio :: StegoParams -> WaveAudio -> IO DecoderResultList
doDecodeWaveAudio stegoParams waveAudio = do
  decoder <- newDecoder stegoParams
  resD <- getResultChannel decoder
  decodeDoneMutex <- newEmptyTMVarIO
  void $ forkIO $ decodeLoop resD [] decodeDoneMutex
  let frames = audioFrames waveAudio
  mapM_ (enqueueFrame decoder) frames
  _ <- stopDecoder decoder
  atomically $ takeTMVar decodeDoneMutex
  where
    decodeLoop channel fs resultVar = do
      res <- atomically $ readTChan channel
      case res of
        StoppingDecoder -> do
          atomically $ putTMVar resultVar (sort fs)
          pure ()
        f -> do
          decodeLoop channel (f : fs) resultVar

-- | Similar to doDecodeWaveAudio. Decodes the provided Frames using an 
-- injected Decoder instance, and returns the DecoderResultList.
doDecodeFramesWithDecoder :: Decoder -> Frames -> IO DecoderResultList
doDecodeFramesWithDecoder decoder frames = do
  resD <- getResultChannel decoder
  decodeDoneMutex <- newEmptyTMVarIO
  void $ forkIO $ decodeLoop resD [] decodeDoneMutex
  mapM_ (enqueueFrame decoder) frames
  _ <- stopDecoder decoder
  atomically $ takeTMVar decodeDoneMutex
  where
    decodeLoop channel fs resultVar = do
      res <- atomically $ readTChan channel
      case res of
        StoppingDecoder -> do
          atomically $ putTMVar resultVar (sort fs)
          pure ()
        f -> do
          decodeLoop channel (f : fs) resultVar
