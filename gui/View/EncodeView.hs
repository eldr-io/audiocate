module View.EncodeView (
  EncodeView (..),
  initEncodeView,
  updateEncodeViewAudioFileLoaded,
)
where

import AppState (AppState (loadedAudio), AppStateLoadedAudio (loadedAudioWave))
import Data.GI.Base
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Gtk qualified as Gtk
import Control.Concurrent (readMVar)
import Data.Audio.Wave (WaveAudio(..))

data EncodeView = EncodeView
  { title :: T.Text
  , id :: T.Text
  , appState :: AppState
  , encodeViewBox :: !Gtk.Box
  , encFilePropSrcLbl :: !Gtk.Label
  -- , encFilePropRateLbl :: !Gtk.Label
  -- , encFilePropBitRateLbl :: !Gtk.Label
  -- , encFilePropSamplesLbl :: !Gtk.Label
  -- , encFilePropNumFramesLbl :: !Gtk.Label
  -- , encFilePropNumChannelsLbl :: !Gtk.Label
  }

updateEncodeViewAudioFileLoaded :: AppState -> EncodeView -> IO ()
updateEncodeViewAudioFileLoaded appState encodeView = do
  putStrLn "update encodeview called"
  audio <- readMVar $ loadedAudio appState
  let wa = loadedAudioWave audio
  let srcLbl = encFilePropSrcLbl encodeView
  Gtk.labelSetLabel srcLbl (T.pack (srcFile wa))


initEncodeView :: AppState -> IO EncodeView
initEncodeView appState = do
  builder <- Gtk.builderNewFromResource "/gui/View/EncodeView.ui"
  encodeBin <- Gtk.builderGetObject builder "encTopBox"
  bin <- castTo Gtk.Box (fromJust encodeBin)

  encFilePropSrcLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropSrcLbl"
  encFilePropSrcLbl <- fromJust <$> castTo Gtk.Label encFilePropSrcLblPtr
  pure
    $ EncodeView "Encode" "encodeView" appState (fromJust bin)
    encFilePropSrcLbl
