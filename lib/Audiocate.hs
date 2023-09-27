module Audiocate (someFunc) where

import Bits.Show (showFiniteBits)
import Control.Monad.Except (runExceptT)
import Data.Audio.Wave (WaveAudio (..), waveAudioFromFile, waveAudioToFile)
import Data.List.Split (chunksOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Stego.Common (EncodingType (LsbEncoding), StegoParams (StegoParams), calculateTotp, checkTotp)
import Stego.Encode.LSB (decodeFrame, encodeFrame)

nanosSinceEpoch :: UTCTime -> Word64
nanosSinceEpoch =
  fromIntegral . floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

someFunc :: IO ()
someFunc = do
  time <- getCurrentTime
  putStrLn $ "Current Time: " ++ show time
  let secret = encodeUtf8 (T.pack "Sef7Kp%IU{T&In-=t'up/V2NwiY7,4Ds")
  putStrLn $ "Acquired secret: " ++ show secret
  let stegoParams = StegoParams secret 30 6 LsbEncoding 123
  putStrLn "Calculating TOTP Value..."
  let totpValue = calculateTotp stegoParams time
  print totpValue
  putStrLn "Verifying TOTP.."
  print $ checkTotp stegoParams time totpValue

  putStrLn "Reading sample audio file.."

  audio <- runExceptT (waveAudioFromFile "test/corpus/harvard.wav")
  case audio of
    Left err -> putStrLn err
    Right wa -> do
      print wa
      let s' = chunksOf (rate wa) $ samples wa
      let s = s' !! 10
      let unixTime = nanosSinceEpoch time
      let encodedFrame = encodeFrame unixTime totpValue s
      putStrLn "BINARY Reps"
      putStrLn $ "Time: " ++ showFiniteBits unixTime
      putStrLn $ "TOTP: " ++ showFiniteBits totpValue
      putStrLn "Encoding file ..."
      let encodedFrames = map (encodeFrame unixTime totpValue) s'
      let combined = concat encodedFrames
      putStrLn "First pass Decoding using LSB 16 SHA1..."
      let decodedFrame = decodeFrame encodedFrame
      print decodedFrame
      putStrLn "Success: True"
      putStrLn "Writing encoded file ..."

      let wa' =
            WaveAudio
              { srcFile = "lol.wav",
                bitSize = 16,
                rate = 88200,
                channels = 1,
                samples = combined
              }
      write <- runExceptT (waveAudioToFile "test/output/harvard_out.wav" wa')
      case write of
        Left err -> putStrLn err
        Right _ -> do
          putStrLn "Wrote file to harvard_out.wav"

          audio' <- runExceptT (waveAudioFromFile "test/output/harvard_out.wav")
          case audio' of
            Left err -> putStrLn err
            Right wd -> do
              print wd
              -- let d = chunksOf (rate wd) $ samples wd
              let d = chunksOf 44100 $ samples wd
              putStrLn $ "Decoding " ++ show (length d) ++ " frame(s)..."
              let ds = map decodeFrame d
              let ts = map (\(x, y) -> x == showFiniteBits unixTime && y == showFiniteBits totpValue) ds
              let success = and ts
              putStrLn $ "Successful Decode: " <> show success
