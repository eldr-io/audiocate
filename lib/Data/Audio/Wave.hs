{-# LANGUAGE TypeApplications #-}

{- | Contains logic for interacting with 16-bit Wave files including
parsing, reading and writing to files and converting to types.
-}
module Data.Audio.Wave (
  WaveAudio (..),
  Sample,
  Samples,
  waveAudioFromFile,
  waveAudioToFile,
)
where

import qualified Codec.Wav (exportFile, importFile)
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT (ExceptT), withExceptT)
import Data.Array.Unboxed (elems, listArray)
import Data.Audio (Audio (Audio))
import Data.Bits (bitSizeMaybe)
import Data.Int (Int16)
import Data.Maybe (fromJust)

type Sample = Int16

type Samples = [Sample]

-- type Frames = [Samples]

-- | Represents raw WaveAudio data parsed from a file
data WaveAudio = WaveAudio
  { rate :: Int
  , channels :: Int
  , samples :: Samples
  , bitSize :: Int
  , srcFile :: FilePath
  }

instance Show WaveAudio where
  show wa =
    "==== WAVE AUDIO ====\n"
      ++ "rate: "
      ++ show (rate wa)
      ++ "\nchannels: "
      ++ show (channels wa)
      ++ "\nnum. samples: "
      ++ show (length $ samples wa)
      ++ "\nsrcFile: "
      ++ srcFile wa
      ++ "\n============="

-- | Helper function for converting Data.Audio Int16 to WaveAudio
toWaveAudio :: FilePath -> Audio Int16 -> WaveAudio
toWaveAudio path (Audio hz chans raw) =
  WaveAudio
    { rate = hz
    , channels = chans
    , samples = elems raw
    , bitSize = fromJust $ bitSizeMaybe (head (elems raw))
    , srcFile = path
    }

{- | Attempts to read the provided Wave file into 16-bit samples
or returns an error string.
-}
readWaveFile :: FilePath -> IO (Either String (Audio Int16))
readWaveFile path = do
  wav <- Codec.Wav.importFile path
  pure (wav :: Either String (Audio Int16))

-- | Wraps the readWaveFile function in ExceptT monad
readWaveFile' :: FilePath -> ExceptT String IO (Audio Int16)
readWaveFile' path = ExceptT (readWaveFile path)

{- | Attempts to read the wave file at the provided filepath into
a full WaveAudio instance.
-}
waveAudioFromFile :: FilePath -> ExceptT String IO WaveAudio
waveAudioFromFile path = toWaveAudio path <$> readWaveFile' path

{- | Writes the provided WaveAudio to a valid .wav file at path or raises
an IOException in the ExcepT monad
-}
waveAudioToFile :: FilePath -> WaveAudio -> ExceptT String IO ()
waveAudioToFile path w = do
  withExceptT (show @IOException)
    $ ExceptT
      ( try
          $ Codec.Wav.exportFile
            path
            ( Audio (rate w) (channels w)
                $ listArray (0, length (samples w))
                $ samples w
            )
      )
