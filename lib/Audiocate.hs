module Audiocate (start) where

import Bits.Show (showFiniteBits)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Data.Audio.Wave (WaveAudio (..), waveAudioFromFile, waveAudioToFile)
import Data.List.Split (chunksOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (StegoParams), calculateTotp, checkTotp, readBinWord32, readBinWord64, utcTimeToWord64, word64ToUtcTime)
import Stego.Encode.Encoder (enqueueFrame, newEncoder, startEncoder, takeStopVar)
import Stego.Encode.LSB (decodeFrame, encodeFrame)

start :: IO ()
start = do
  let inputFile = "test/corpus/sample1.wav"
  time <- getCurrentTime
  putStrLn $ "Current Time: " ++ show time
  let secret = encodeUtf8 (T.pack "Sef7Kp%IU{T&In-=t'up/V2NwiY7,4Ds")
  putStrLn $ "Acquired secret: " ++ show secret
  let stegoParams = StegoParams secret 5 6 LsbEncoding 123
  putStrLn "Starting encoder.."
  encoder <- newEncoder stegoParams
  putStrLn "Reading sample audio file.."
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> putStrLn err
    Right wa -> do
      let s' = chunksOf (rate wa) $ samples wa
      putStrLn $ "Enqueueing " ++ show (length s') ++ " frames.."
      mapM_
        ( \x -> do
            putStrLn "Enqueue..."
            enqueueFrame encoder x
        )
        (init s')
      startEncoder encoder
      threadDelay 5000
      putStrLn "Enqueue last"
      enqueueFrame encoder (last s')
      frames <- takeStopVar encoder
      print (length frames)
      print $ length frames == length s'
      let df = decodeFrame (head frames)
      let decodedTimestamp = readBinWord64 (fst df)
      let decodedTotpValue = readBinWord32 (snd df)
      print decodedTimestamp
      print decodedTotpValue
      let dstamp = word64ToUtcTime decodedTimestamp
      print dstamp
      let pog = checkTotp stegoParams dstamp decodedTotpValue
      putStrLn $ "SUCCESS: " ++ show pog

someFunc :: IO ()
someFunc = do
  let inputFile = "test/corpus/sample2.wav"
  let outputFile = "test/output/sample2_out.wav"
  time <- getCurrentTime
  putStrLn $ "Current Time: " ++ show time
  let secret = encodeUtf8 (T.pack "Sef7Kp%IU{T&In-=t'up/V2NwiY7,4Ds")
  putStrLn $ "Acquired secret: " ++ show secret
  let stegoParams = StegoParams secret 5 6 LsbEncoding 123
  putStrLn "Calculating TOTP Value..."
  let totpValue = calculateTotp stegoParams time
  print totpValue
  putStrLn "Verifying TOTP.."
  print $ checkTotp stegoParams time totpValue

  putStrLn "Reading sample audio file.."
  audio <- runExceptT (waveAudioFromFile inputFile)
  case audio of
    Left err -> putStrLn err
    Right wa -> do
      print wa
      let s' = chunksOf (rate wa) $ samples wa
      let s = s' !! 10
      let timestamp = utcTimeToWord64 time
      let encodedFrame = encodeFrame timestamp totpValue s
      putStrLn "BINARY Reps"
      putStrLn $ "Time: " ++ showFiniteBits timestamp
      putStrLn $ "TOTP: " ++ showFiniteBits totpValue
      putStrLn "Encoding file ..."
      let encodedFrames = map (encodeFrame timestamp totpValue) s'
      let combined = concat encodedFrames
      putStrLn "First pass Decoding using LSB 16 SHA1..."
      let decodedFrame = decodeFrame encodedFrame
      print decodedFrame
      let decodedTimestamp = readBinWord64 (fst decodedFrame)
      let decodedTotpValue = readBinWord32 (snd decodedFrame)
      print decodedTimestamp
      print decodedTotpValue
      let dstamp = word64ToUtcTime decodedTimestamp
      print dstamp
      let pog = checkTotp stegoParams dstamp decodedTotpValue
      putStrLn $ "Success: " ++ show pog
      putStrLn "Writing encoded file ..."

      let wa' =
            WaveAudio
              { srcFile = outputFile,
                bitSize = 16,
                rate = rate wa,
                channels = channels wa,
                samples = combined
              }
      write <- runExceptT (waveAudioToFile outputFile wa')
      case write of
        Left err -> putStrLn err
        Right _ -> do
          putStrLn $ "Wrote file to " ++ outputFile

          audio' <- runExceptT (waveAudioFromFile outputFile)
          case audio' of
            Left err -> putStrLn err
            Right wd -> do
              print wd
              let d = chunksOf (rate wd) $ samples wd
              putStrLn $ "Decoding " ++ show (length d) ++ " frame(s)..."
              let ds = map decodeFrame d
              let ts = map (\(x, y) -> x == showFiniteBits timestamp && y == showFiniteBits totpValue) ds
              let success = and ts
              putStrLn $ "Successful Decode: " <> show success
