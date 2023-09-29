module Audiocate (start) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTChan, takeTMVar)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.Audio.Wave (WaveAudio (..), waveAudioFromFile, waveAudioToFile, Frames)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (StegoParams))
import Stego.Decode.Decoder qualified as DC
import Stego.Encode.Encoder (EncoderResult (..), enqueueFrame, getResultChannel, newEncoder, stopEncoder)
import Data.List (sort)
import Stego.Decode.Decoder (DecoderResultList)

doEncodeFrames :: StegoParams -> Frames -> IO DecoderResultList
doEncodeFrames stegoParams frames = do
  encoder <- newEncoder stegoParams
  resC <- getResultChannel encoder
  printChan <- getResultChannel encoder
  decoder <- DC.newDecoder stegoParams
  void $ DC.mapDecoderOpQToResultChan decoder resC
  resD <- DC.getResultChannel decoder
  x <- newEmptyTMVarIO
  void $ forkIO $ loopDc resD [] (length frames) x
  void $ forkIO $ printLoop printChan 0 (length frames)

  mapM_ (enqueueFrame encoder) frames
  t <- stopEncoder encoder
  void $ atomically $ takeTMVar t
  atomically $ takeTMVar x
 where
  loopDc resD fs total t = do
    res <- atomically $ readTChan resD
    case res of
      DC.StoppingDecoder -> do
        void $ atomically $ putTMVar t fs
      f -> do
        loopDc resD (f : fs) total t
  printLoop c fs totalFs = do
    res <- atomically $ readTChan c
    case res of
      StoppingEncoder -> do 
        pure ()
      _ -> do
        printLoop c (fs + 1) totalFs

doDecodeWaveAudio :: StegoParams -> WaveAudio -> IO DC.DecoderResultList
doDecodeWaveAudio stegoParams waveAudio = do
  decoder <- DC.newDecoder stegoParams
  resD <- DC.getResultChannel decoder
  m <- newEmptyTMVarIO
  void $ forkIO $ decodeLoop resD [] m
  let frames = audioFrames waveAudio
  mapM_ (DC.enqueueFrame decoder) frames
  _ <- DC.stopDecoder decoder
  atomically $ takeTMVar m
 where
  decodeLoop channel fs resultVar = do
    res <- atomically $ readTChan channel
    case res of
      DC.StoppingDecoder -> do
        atomically $ putTMVar resultVar (sort fs)
        pure ()
      f -> do
        decodeLoop channel (f : fs) resultVar

start :: IO ()
start = do
  let inputFile = "test/corpus/sample2.wav"
  let outputFile = "test/output/sample2_out.wav"
  let secret = encodeUtf8 (T.pack "Sef7Kp%IU{T&In-=t'up/V2NwiY7,4Ds")
  let stegoParams = StegoParams secret 5 6 LsbEncoding 123
  doEncodeFile stegoParams inputFile outputFile
  doDecodeFile stegoParams outputFile

doDecodeFile :: StegoParams -> FilePath -> IO ()
doDecodeFile stegoParams inputFile = do
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
      print $ DC.getResultStats result
      endTime <- getCurrentTime
      putStrLn $ "Completed decode in " <> show (diffUTCTime endTime startTime)

doEncodeFile :: StegoParams -> FilePath -> FilePath -> IO ()
doEncodeFile stegoParams inputFile outputFile = do
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
      let frames = audioFrames wa
      result <- doEncodeFrames stegoParams (take (length frames `div` 2) frames)
      result2 <- doEncodeFrames stegoParams (drop (length frames `div` 2) frames)
      putStrLn "\nEncode Result "
      putStrLn $ "\nTotal Frames in file: " ++ show (length $ audioFrames wa)
      let combined = result ++ result2
      print $ DC.getResultStats combined
      putStrLn $ "Writing encoded file " ++ outputFile ++ "..."
      let wa' =
            WaveAudio
              { srcFile = outputFile
              , bitSize = 16
              , rate = rate wa
              , channels = channels wa
              , audioFrames = []
              , samples = concatMap snd (sort $ DC.getResultFrames combined)
              }
      write <- runExceptT (waveAudioToFile outputFile wa')
      case write of
        Left err -> putStrLn err
        Right _ -> do
          endTime <- getCurrentTime
          putStrLn $ "Completed encode in " <> show (diffUTCTime endTime startTime)
