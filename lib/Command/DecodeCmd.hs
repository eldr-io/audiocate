module Command.DecodeCmd (
 runDecodeCmd,
) where

import Stego.Common (StegoParams(..))
import Data.Audio.Wave (WaveAudio (..), waveAudioFromFile)
import Stego.Decode.Decoder (DecoderResultList, DecoderResult (StoppingDecoder), getResultStats, newDecoder, getResultChannel, enqueueFrame, stopDecoder)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad.Except (runExceptT)
import Control.Concurrent.STM (newEmptyTMVarIO, atomically, takeTMVar, readTChan, putTMVar)
import Control.Monad ( void )
import Data.List (sort)
import Control.Concurrent (forkIO)

runDecodeCmd :: StegoParams -> FilePath -> IO ()
runDecodeCmd stegoParams inputFile = do
  startTime <- getCurrentTime
  putStrLn $ "Reading audio file " ++ inputFile ++ "..." 
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> putStrLn err
    Right wa -> do
      readTime <- getCurrentTime
      putStrLn $ "Read file in " <> show (diffUTCTime readTime startTime)
      putStrLn ""
      print wa
      result <- doDecodeWaveAudio stegoParams wa
      putStrLn "\nDecode Result "
      print $ getResultStats result
      endTime <- getCurrentTime
      putStrLn $ "Completed decode in " <> show (diffUTCTime endTime startTime)

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
