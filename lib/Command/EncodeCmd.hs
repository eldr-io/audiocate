module Command.EncodeCmd (
  runEncodeCmd
) where

import Stego.Common
import qualified Stego.Decode.Decoder as DC
import Data.Audio.Wave (WaveAudio(..), Frames, waveAudioToFile, waveAudioFromFile)
import Stego.Encode.Encoder (EncoderResult(..), newEncoder, getResultChannel, enqueueFrame, stopEncoder)
import Control.Monad (void)
import Control.Concurrent.STM (newEmptyTMVarIO, takeTMVar, readTChan, putTMVar)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad.Except (runExceptT)
import Data.List (sort)


runEncodeCmd :: StegoParams -> FilePath -> FilePath -> IO ()
runEncodeCmd stegoParams inputFile outputFile = do
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
              , bitSize = bitSize wa
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

doEncodeFrames :: StegoParams -> Frames -> IO DC.DecoderResultList
doEncodeFrames stegoParams frames = do
  encoder <- newEncoder stegoParams
  resC <- getResultChannel encoder
  printChan <- getResultChannel encoder
  decoder <- DC.newDecoder stegoParams
  void $ DC.mapDecoderOpQToResultChan decoder resC
  resD <- DC.getResultChannel decoder
  x <- newEmptyTMVarIO
  void $ forkIO $ decoderResultLoop resD [] (length frames) x
  void $ forkIO $ printLoop printChan (0 :: Int) (length frames)

  mapM_ (enqueueFrame encoder) frames
  t <- stopEncoder encoder
  void $ atomically $ takeTMVar t
  atomically $ takeTMVar x
 where
  decoderResultLoop resD fs total t = do
    res <- atomically $ readTChan resD
    case res of
      DC.StoppingDecoder -> do
        void $ atomically $ putTMVar t fs
      f -> do
        decoderResultLoop resD (f : fs) total t
  printLoop c fs totalFs = do
    res <- atomically $ readTChan c
    case res of
      StoppingEncoder -> do 
        pure ()
      _ -> do
        printLoop c (fs + 1) totalFs
