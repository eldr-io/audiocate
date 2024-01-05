{-# LANGUAGE TypeApplications #-}

-- | Contains logic for interacting with 16-bit Wave files including
-- parsing, reading and writing to files and converting to types.
module Data.Audio.Wave
  ( WaveAudio(..)
  , Sample
  , Samples
  , Frame
  , Frames
  , waveAudioFromFile
  , waveAudioToFile
  ) where

import qualified Codec.Wav (exportFile, importFile)
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT(ExceptT), withExceptT)
import Data.Array.Unboxed (elems, listArray)
import Data.Audio (Audio(Audio))
import Data.Bits (bitSizeMaybe)
import Data.Int (Int16)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

type Sample = Int16

type Samples = [Sample]

type Frame = (Int, [Sample])

type Frames = [Frame]

-- | Represents raw WaveAudio data parsed from a file
data WaveAudio =
  WaveAudio
    { rate :: Int
    , channels :: Int
    , samples :: Samples
    , audioFrames :: Frames
    , bitSize :: Int
    , srcFile :: FilePath
    }

instance Show WaveAudio where
  show wa =
    "WAVE audio file\n========================\n" ++
    " rate: " ++
    show (rate wa) ++
    "\n channels: " ++
    show (channels wa) ++
    "\n num. samples: " ++
    show (length $ samples wa) ++
    "\n srcFile: " ++ srcFile wa ++ "\n========================"

-- | Helper function for converting Data.Audio Int16 to WaveAudio
toWaveAudio :: FilePath -> Audio Int16 -> WaveAudio
toWaveAudio path (Audio hz chans raw) =
  WaveAudio
    { rate = hz
    , channels = chans
    , samples = elems raw
    , audioFrames = zip [0 .. length chunks - 1] chunks
    , bitSize = fromJust $ bitSizeMaybe (head (elems raw))
    , srcFile = path
    }
  where
    chunks = chunksOf hz (elems raw)

-- | Attempts to read the provided Wave file into 16-bit samples
-- or returns an error string.
readWaveFile :: FilePath -> IO (Either String (Audio Int16))
readWaveFile path = do
  wav <- Codec.Wav.importFile path
  pure (wav :: Either String (Audio Int16))

-- | Wraps the readWaveFile function in ExceptT monad
readWaveFile' :: FilePath -> ExceptT String IO (Audio Int16)
readWaveFile' path = ExceptT (readWaveFile path)

-- | Attempts to read the wave file at the provided filepath into
-- a full WaveAudio instance.
waveAudioFromFile :: FilePath -> ExceptT String IO WaveAudio
waveAudioFromFile path = toWaveAudio path <$> readWaveFile' path

-- | Writes the provided WaveAudio to a valid .wav file at path or raises
-- an IOException in the ExcepT monad
waveAudioToFile :: FilePath -> WaveAudio -> ExceptT String IO ()
waveAudioToFile path w = do
  withExceptT (show @IOException) $
    ExceptT
      (try $
       Codec.Wav.exportFile
         path
         (Audio (rate w) (channels w) $
          listArray (0, length (samples w)) $ samples w))
