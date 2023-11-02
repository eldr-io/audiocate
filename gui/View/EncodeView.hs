module View.EncodeView
  ( EncodeView (..),
    initEncodeView,
    updateEncodeViewAudioFileLoaded,
  )
where

import AppState (AppState (loadedAudio), AppStateLoadedAudio (loadedAudioWave))
import Control.Concurrent (readMVar)
import Data.Audio.Wave (WaveAudio (..))
import Data.GI.Base
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import Data.Word (Word64)
import Data.Text.Encoding (encodeUtf8)
import Stego.Common (StegoParams(..), EncodingType(LsbEncoding))
import Command.EncodeCmd (runEncodeCmd)

data EncodeView = EncodeView
  { title :: T.Text,
    id :: T.Text,
    appState :: AppState,
    encodeViewBox :: !Gtk.Box,
    topBanner :: !Adw.Banner,
    encFilePropSrcLbl :: !Gtk.Label,
    encFilePropRateLbl :: !Gtk.Label,
    encFilePropBitRateLbl :: !Gtk.Label,
    encFilePropSamplesLbl :: !Gtk.Label,
    encFilePropNumFramesLbl :: !Gtk.Label,
    encFilePropNumChannelsLbl :: !Gtk.Label,
    secretKeyEntryRow :: !Adw.EntryRow,
    -- payloadEntryRow :: !Adw.EntryRow,
    secondsValidEntryRow :: !Adw.EntryRow,
    runEncoderBtn :: !Gtk.Button,
    toastOverlay :: !Adw.ToastOverlay
  }

-- | Updates the EncodeView listbox with the properties of the loaded
-- newly audio file
updateEncodeViewAudioFileLoaded :: AppState -> EncodeView -> IO ()
updateEncodeViewAudioFileLoaded appState encodeView = do
  audio <- readMVar $ loadedAudio appState
  let wa = loadedAudioWave audio
  let srcLbl = encFilePropSrcLbl encodeView
  Gtk.labelSetLabel srcLbl (T.pack (srcFile wa))
  let rateLbl = encFilePropRateLbl encodeView
  Gtk.labelSetLabel rateLbl (T.pack $ show (rate wa))
  let bitRateLbl = encFilePropBitRateLbl encodeView
  Gtk.labelSetLabel bitRateLbl (T.pack $ show (bitSize wa))
  let samplesLbl = encFilePropSamplesLbl encodeView
  Gtk.labelSetLabel samplesLbl (T.pack $ show (length $ samples wa))
  let numFramesLbl = encFilePropNumFramesLbl encodeView
  Gtk.labelSetLabel numFramesLbl (T.pack $ show (length $ audioFrames wa))
  let numChannelsLbl = encFilePropNumChannelsLbl encodeView
  Gtk.labelSetLabel numChannelsLbl (T.pack $ show (channels wa))
  let banner = topBanner encodeView
  Adw.bannerSetRevealed banner False
  let runEncodeBtn = runEncoderBtn encodeView 
  Gtk.setWidgetSensitive runEncodeBtn True

-- | Handler for the onClicked event of the RunEncoderBtn
onEncodeBtnClicked :: AppState -> EncodeView -> IO ()
onEncodeBtnClicked appState encodeView = do
  putStrLn "encode btn clicked"
  audio <- readMVar $ loadedAudio appState
  let inputFile = srcFile $ loadedAudioWave audio
  let secretKey = secretKeyEntryRow encodeView
  secret <- Gtk.editableGetText secretKey
  secondsValid <- Gtk.editableGetText (secondsValidEntryRow encodeView)
  print secret
  print secondsValid
  let secondsValidInt :: Int = read (T.unpack secondsValid)
  if secret == "" then do
    putStrLn "invalid secret specified"
    toast <-
      new
        Adw.Toast
        [ #timeout := 2,
          #title := "Invalid secret specified. Please specify a valid Secret Key."
        ]
    Adw.toastOverlayAddToast (toastOverlay encodeView) toast
    pure ()
  else if secondsValid == "" || secondsValidInt < 1 then do
    putStrLn "invalid seconds valid"
    toast <-
      new
        Adw.Toast
        [ #timeout := 2,
          #title := "Invalid seconds valid. Please specify the validity seconds window."
        ]
    Adw.toastOverlayAddToast (toastOverlay encodeView) toast
    pure ()
  else do
    -- do encode
    let s = encodeUtf8 secret
    let t :: Word64 = fromIntegral secondsValidInt
    let stegoParams = StegoParams s t 6 LsbEncoding 123
    runEncodeCmd stegoParams inputFile "tmp.wav"

-- | Initialises the encodeView by loading the .ui resource and creating
-- the EncodeView data instance
initEncodeView :: AppState -> Adw.ToastOverlay -> IO EncodeView
initEncodeView appState overlay = do
  builder <- Gtk.builderNewFromResource "/gui/View/EncodeView.ui"
  encodeBin <- Gtk.builderGetObject builder "encTopBox"
  bin <- fromJust <$> castTo Gtk.Box (fromJust encodeBin)

  -- build banner instance
  bannerPtr <- fromJust <$> Gtk.builderGetObject builder "banner"
  banner <- fromJust <$> castTo Adw.Banner bannerPtr

  -- build Label instances
  encFilePropSrcLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropSrcLbl"
  encFilePropSrcLbl <- fromJust <$> castTo Gtk.Label encFilePropSrcLblPtr
  encFilePropRateLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropRateLbl"
  encFilePropRateLbl <- fromJust <$> castTo Gtk.Label encFilePropRateLblPtr
  encFilePropBitRateLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropBitRateLbl"
  encFilePropBitRateLbl <- fromJust <$> castTo Gtk.Label encFilePropBitRateLblPtr
  encFilePropSamplesLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropSamplesLbl"
  encFilePropSamplesLbl <- fromJust <$> castTo Gtk.Label encFilePropSamplesLblPtr
  encFilePropNumFramesLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropNumFramesLbl"
  encFilePropNumFramesLbl <- fromJust <$> castTo Gtk.Label encFilePropNumFramesLblPtr
  encFilePropNumChannelsLblPtr <- fromJust <$> Gtk.builderGetObject builder "encFilePropNumChannelsLbl"
  encFilePropNumChannelsLbl <- fromJust <$> castTo Gtk.Label encFilePropNumChannelsLblPtr
  -- build EntryRow instances
  secretKeyEntryRowPtr <- fromJust <$> Gtk.builderGetObject builder "secretKeyEntryRow"
  secretKeyEntryRow <- fromJust <$> castTo Adw.EntryRow secretKeyEntryRowPtr
  secondsValidEntryRowPtr <- fromJust <$> Gtk.builderGetObject builder "secondsValidEntryRow"
  secondsValidEntryRow <- fromJust <$> castTo Adw.EntryRow secondsValidEntryRowPtr
  -- build button instances
  runEncoderBtnPtr <- fromJust <$> Gtk.builderGetObject builder "runEncoderBtn"
  runEncoderBtn <- fromJust <$> castTo Gtk.Button runEncoderBtnPtr

  -- build eventView state ADT
  let ev =
        EncodeView
          "Encode"
          "encodeView"
          appState
          bin
          banner
          encFilePropSrcLbl
          encFilePropRateLbl
          encFilePropBitRateLbl
          encFilePropSamplesLbl
          encFilePropNumFramesLbl
          encFilePropNumChannelsLbl
          secretKeyEntryRow
          secondsValidEntryRow
          runEncoderBtn
          overlay

  -- | register event handlers
  Adw.after runEncoderBtn #clicked $ do
    onEncodeBtnClicked appState ev

  pure ev
