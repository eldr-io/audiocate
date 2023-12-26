module Command.EncodeCmd
  ( runEncodeCmd
  , doEncodeFramesWithEncoder
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newEmptyTMVarIO, putTMVar, readTChan, takeTMVar)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.STM (atomically)
import Data.List (sort)
import Data.Time (diffUTCTime, getCurrentTime)

import Data.Audio.Wave
  ( Frames
  , WaveAudio(..)
  , waveAudioFromFile
  , waveAudioToFile
  )
import Stego.Common (StegoParams)
import qualified Stego.Decode.Decoder as DC
import Stego.Encode.Encoder
  ( Encoder(..)
  , EncoderResult(..)
  , enqueueFrame
  , getResultChannel
  , newEncoder
  , stopEncoder
  )

runEncodeCmd ::
     StegoParams
  -> FilePath
  -> FilePath
  -> IO (Either String DC.DecoderResultList)
runEncodeCmd stegoParams inputFile outputFile = do
  startTime <- getCurrentTime
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> pure (Left err)
    Right wa -> do
      let frames = audioFrames wa
      result <- doEncodeFrames stegoParams (take (length frames `div` 2) frames)
      result2 <-
        doEncodeFrames stegoParams (drop (length frames `div` 2) frames)
      putStrLn "\nEncode Result "
      putStrLn $ "\nTotal Frames in file: " ++ show (length $ audioFrames wa)
      let combined = result ++ result2
      let resultStats = DC.getResultStats combined
      print resultStats
      putStrLn $ "Writing encoded file to " ++ outputFile ++ "..."
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
        Left err -> pure (Left err)
        Right _ -> do
          endTime <- getCurrentTime
          putStrLn $
            "Completed encode in " <> show (diffUTCTime endTime startTime)
          pure (Right combined)

doEncodeFrames :: StegoParams -> Frames -> IO DC.DecoderResultList
doEncodeFrames stegoParams frames = do
  encoder <- newEncoder stegoParams
  resC <- getResultChannel encoder
  printChan <- getResultChannel encoder
  decoder <- DC.newDecoder stegoParams
  void $ DC.mapDecoderOpQToResultChan decoder resC
  resD <- DC.getResultChannel decoder
  decoderStoppedTMVar <- newEmptyTMVarIO
  void $ forkIO $ decoderResultLoop resD [] (length frames) decoderStoppedTMVar
  void $ forkIO $ printLoop printChan (0 :: Int) (length frames)
  mapM_ (enqueueFrame encoder) frames
  t <- stopEncoder encoder
  void $ atomically $ takeTMVar t
  atomically $ takeTMVar decoderStoppedTMVar
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

-- | Performs an Encoding on the provided Frames using the provided Encoder instance
doEncodeFramesWithEncoder :: Encoder -> Frames -> IO DC.DecoderResultList
doEncodeFramesWithEncoder encoder frames = do
  let stegoPs = stegoParams encoder
  resC <- getResultChannel encoder
  decoder <- DC.newDecoder stegoPs
  void $ DC.mapDecoderOpQToResultChan decoder resC
  resDecodeChan <- DC.getResultChannel decoder
  decodeMVar <- newEmptyTMVarIO
  void $ forkIO $ decoderResultLoop resDecodeChan [] (length frames) decodeMVar
  mapM_ (enqueueFrame encoder) frames
  stopMVar <- stopEncoder encoder
  void $ atomically $ takeTMVar stopMVar
  atomically $ takeTMVar decodeMVar
  where
    decoderResultLoop resDChan fs total t = do
      res <- atomically $ readTChan resDChan
      case res of
        DC.StoppingDecoder -> do
          void $ atomically $ putTMVar t fs
        f -> do
          decoderResultLoop resDChan (f : fs) total t
