module Audiocate (start) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTChan, takeTMVar)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.Audio.Wave (Frames, WaveAudio (..), waveAudioFromFile, waveAudioToFile)
import Data.List.Split (chunksOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (StegoParams))
import Stego.Decode.Decoder qualified as DC
import Stego.Encode.Encoder (EncoderResult (..), enqueueFrame, getResultChannel, newEncoder, stopEncoder)

doEncodeWaveAudio :: StegoParams -> WaveAudio -> IO Frames
doEncodeWaveAudio stegoParams waveAudio = do
  encoder <- newEncoder stegoParams
  decoder <- DC.newDecoder stegoParams
  resD <- DC.getResultChannel decoder
  resC <- getResultChannel encoder
  resPrint <- getResultChannel encoder
  let frames = chunksOf (rate waveAudio) (samples waveAudio)

  void $ DC.mapDecoderOpQToResultChan decoder resC
  void $ forkIO $ printLoop resPrint (0 :: Int) frames

  mapM_ (enqueueFrame encoder) frames
  _ <- stopEncoder encoder
  loopDc resD [] (length frames)
 where
  loopDc resD fs total = do
    res <- atomically $ readTChan resD
    case res of
      (DC.DecodedFrameR (f, _) verified) -> do
        putStrLn
          $ "Decoded: "
          ++ show (length fs + 1)
          ++ "/"
          ++ show total
          ++ " (Verified: "
          ++ show verified
          ++ ")"
        loopDc resD (f : fs) total
      DC.StoppingDecoder -> do
        putStrLn "Received stopping decoder"
        pure (reverse fs)
      DC.SkippedFrame f -> do
        loopDc resD (f : fs) total
  printLoop c fs totalFs = do
    res <- atomically $ readTChan c
    case res of
      StoppingEncoder -> pure ()
      _ -> do
        putStrLn
          $ "Encoded "
          ++ show (fs + 1)
          ++ "/"
          ++ show (length totalFs)
          ++ " frame(s)."
        printLoop c (fs + 1) totalFs

doDecodeWaveAudio :: StegoParams -> WaveAudio -> IO DC.DecoderResultList
doDecodeWaveAudio stegoParams waveAudio = do
  decoder <- DC.newDecoder stegoParams
  resD <- DC.getResultChannel decoder
  m <- newEmptyTMVarIO
  void $ forkIO $ decodeLoop resD [] m
  let frames = chunksOf (rate waveAudio) (samples waveAudio)
  print $ length frames
  mapM_ (DC.enqueueFrame decoder) frames
  _ <- DC.stopDecoder decoder
  atomically $ takeTMVar m
 where
  decodeLoop channel fs resultVar = do
    res <- atomically $ readTChan channel
    case res of
      DC.StoppingDecoder -> do
        putStrLn "Received stopping decoder"
        atomically $ putTMVar resultVar (reverse fs)
        pure ()
      f -> do
        decodeLoop channel (f : fs) resultVar

start :: IO ()
start = do
  let inputFile = "test/corpus/sample1.wav"
  let outputFile = "test/output/sample1_out.wav"
  doEncodeFile inputFile outputFile
  putStrLn "Time for decode .."
  doDecodeFile outputFile

doDecodeFile :: FilePath -> IO ()
doDecodeFile inputFile = do
  time <- getCurrentTime
  putStrLn $ "Current Time: " ++ show time
  let secret = encodeUtf8 (T.pack "Sef7Kp%IU{T&In-=t'up/V2NwiY7,4Ds")
  putStrLn $ "Acquired secret: " ++ show secret
  let stegoParams = StegoParams secret 5 6 LsbEncoding 123
  putStrLn "Reading sample audio file.."
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> putStrLn err
    Right wa -> do
      putStrLn "Starting decoder.."
      result <- doDecodeWaveAudio stegoParams wa
      putStrLn $ "\n\nDecode Result for " ++ inputFile
      print $ DC.getResultStats result

doEncodeFile :: FilePath -> FilePath -> IO ()
doEncodeFile inputFile outputFile = do
  time <- getCurrentTime
  putStrLn $ "Current Time: " ++ show time
  let secret = encodeUtf8 (T.pack "Sef7Kp%IU{T&In-=t'up/V2NwiY7,4Ds")
  putStrLn $ "Acquired secret: " ++ show secret
  let stegoParams = StegoParams secret 5 6 LsbEncoding 123
  putStrLn "Reading sample audio file.."
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> putStrLn err
    Right wa -> do
      putStrLn "Starting decoder.."
      result <- doEncodeWaveAudio stegoParams wa
      putStrLn "Done."
      putStrLn $ "Writing encoded file " ++ outputFile ++ "..."
      let wa' =
            WaveAudio
              { srcFile = outputFile
              , bitSize = 16
              , rate = rate wa
              , channels = channels wa
              , samples = concat result
              }
      write <- runExceptT (waveAudioToFile outputFile wa')
      case write of
        Left err -> putStrLn err
        Right _ -> putStrLn "wrote file"
