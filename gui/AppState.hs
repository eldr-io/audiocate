module AppState (
  AppState (..),
  AppStateLoadedAudio (..),
  newAppState,
) where

import Control.Concurrent (MVar, newEmptyMVar)
import Data.Audio.Wave (WaveAudio)

data AppStateLoadedAudio = AppStateLoadedAudio
  { loadedAudioWave :: WaveAudio
  , loadedAudioFile :: Bool
  , loadedAudioFilePath :: FilePath
  }

data AppState = AppState
  { loadedAudio :: MVar AppStateLoadedAudio
  }

-- | Creates a new empty appState
newAppState :: IO AppState
newAppState = do
  AppState <$> newEmptyMVar
