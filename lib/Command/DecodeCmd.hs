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

doDecodeWaveAudio :: StegoParams -> WaveAudio -> IO DecoderResultList
doDecodeWaveAudio stegoParams waveAudio = do
  decoder <- newDecoder stegoParams
  resD <- getResultChannel decoder
  m <- newEmptyTMVarIO
  void $ forkIO $ decodeLoop resD [] m
  let frames = audioFrames waveAudio
  mapM_ (enqueueFrame decoder) frames
  _ <- stopDecoder decoder
  atomically $ takeTMVar m
  where
    decodeLoop channel fs resultVar = do
      res <- atomically $ readTChan channel
      case res of
        StoppingDecoder -> do
          atomically $ putTMVar resultVar (sort fs)
          pure ()
        f -> do
          decodeLoop channel (f : fs) resultVar

doDecodeFramesWithDecoder :: Decoder -> Frames -> IO DecoderResultList
doDecodeFramesWithDecoder decoder frames = do
  resD <- getResultChannel decoder
  m <- newEmptyTMVarIO
  void $ forkIO $ decodeLoop resD [] m
  mapM_ (enqueueFrame decoder) frames
  _ <- stopDecoder decoder
  atomically $ takeTMVar m
  where
    decodeLoop channel fs resultVar = do
      res <- atomically $ readTChan channel
      case res of
        StoppingDecoder -> do
          atomically $ putTMVar resultVar (sort fs)
          pure ()
        f -> do
          decodeLoop channel (f : fs) resultVar
