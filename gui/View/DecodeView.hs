module View.DecodeView
  ( DecodeView(..)
  , initDecodeView
  , updateDecodeViewAudioFileLoaded
  ) where

import AppState (AppState(loadedAudio), AppStateLoadedAudio(loadedAudioWave))
import Command.DecodeCmd (doDecodeFramesWithDecoder)
import Control.Concurrent (forkIO, readMVar, threadDelay)
import Control.Concurrent.STM (atomically, readTChan)
import Control.Monad (void)
import Data.Audio.Wave (WaveAudio(..))
import Data.GI.Base (AttrOp((:=)), castTo, new)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import qualified GI.Adw as Adw
import qualified GI.Gtk as Gtk
import Stego.Common
  ( EncodingType(LsbEncoding)
  , StegoParams(..)
  , shouldSkipFrame
  )
import Stego.Decode.Decoder
  ( DecoderResult(DecodedFrameR, SkippedFrame, StoppingDecoder)
  , getResultChannel
  , getResultStats
  , newDecoder
  )

data DecodeView =
  DecodeView
    { title :: T.Text
    , id :: T.Text
    , appState :: AppState
    , decodeViewBox :: !Gtk.Box
    , topBanner :: !Adw.Banner
    , encFilePropSrcLbl :: !Gtk.Label
    , encFilePropRateLbl :: !Gtk.Label
    , encFilePropBitRateLbl :: !Gtk.Label
    , encFilePropSamplesLbl :: !Gtk.Label
    , encFilePropNumFramesLbl :: !Gtk.Label
    , encFilePropNumChannelsLbl :: !Gtk.Label
    , secretKeyEntryRow :: !Adw.EntryRow
    -- payloadEntryRow :: !Adw.EntryRow,
    , secondsValidEntryRow :: !Adw.EntryRow
    , runDecoderBtn :: !Gtk.Button
    , framesWindow :: !Gtk.ScrolledWindow
    , framesContainer :: !Gtk.Box
    , toastOverlay :: !Adw.ToastOverlay
    , outputTextView :: !Gtk.TextView
    }

-- | Updates the DecodeView listbox with the properties of the loaded
-- newly audio file
updateDecodeViewAudioFileLoaded :: AppState -> DecodeView -> IO ()
updateDecodeViewAudioFileLoaded appState decodeView = do
  audio <- readMVar $ loadedAudio appState
  let wa = loadedAudioWave audio
  let srcFilePath = T.pack (srcFile wa)
  let srcLbl = encFilePropSrcLbl decodeView
  Gtk.labelSetLabel srcLbl srcFilePath
  let rateLbl = encFilePropRateLbl decodeView
  Gtk.labelSetLabel rateLbl (T.pack $ show (rate wa))
  let bitRateLbl = encFilePropBitRateLbl decodeView
  Gtk.labelSetLabel bitRateLbl (T.pack $ show (bitSize wa))
  let samplesLbl = encFilePropSamplesLbl decodeView
  Gtk.labelSetLabel samplesLbl (T.pack $ show (length $ samples wa))
  let numFramesLbl = encFilePropNumFramesLbl decodeView
  Gtk.labelSetLabel numFramesLbl (T.pack $ show (length $ audioFrames wa))
  let numChannelsLbl = encFilePropNumChannelsLbl decodeView
  Gtk.labelSetLabel numChannelsLbl (T.pack $ show (channels wa))
  let banner = topBanner decodeView
  Adw.bannerSetRevealed banner False
  -- clear the output textview
  let textView = outputTextView decodeView
  textBuffer <- Gtk.textViewGetBuffer textView
  startIter <- Gtk.textBufferGetStartIter textBuffer
  endIter <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferDelete textBuffer startIter endIter
  let runDecodeBtn = runDecoderBtn decodeView
  let framesBox = framesContainer decodeView
  let frames = audioFrames wa
  -- Generate the visible frame indicators and append them to the framesBox
  mapM_
    (\(x, y) -> do
       let lbl :: T.Text = T.pack (show (x + 1))
       let cardColour =
             (if shouldSkipFrame (x, y)
                then "osd"
                else "")
       let lblColour =
             (if cardColour == "osd"
                then "osd"
                else "success")
       lblInstance <-
         new
           Gtk.Label
           [#label := lbl, #widthRequest := 55, #cssClasses := [lblColour]]
       item <-
         new
           Adw.Bin
           [ #child := lblInstance
           , #marginTop := 6
           , #marginBottom := 6
           , #marginStart := 6
           , #marginEnd := 6
           , #cssClasses := ["card", cardColour]
           ]
       Gtk.boxAppend framesBox item)
    frames
  Gtk.setWidgetSensitive runDecodeBtn True

-- | Handler for the onClicked event of the RunDecoderBtn
onDecodeBtnClicked :: AppState -> DecodeView -> IO ()
onDecodeBtnClicked appState decodeView = do
  audio <- readMVar $ loadedAudio appState
  let secretKey = secretKeyEntryRow decodeView
  secret <- Gtk.editableGetText secretKey
  secondsValid <- Gtk.editableGetText (secondsValidEntryRow decodeView)
  print secret
  print secondsValid
  let secondsValidInt :: Int = read (T.unpack secondsValid)
  if secret == ""
    then do
      putStrLn "invalid secret specified"
      toast <-
        Data.GI.Base.new
          Adw.Toast
          [ #timeout := 2
          , #title :=
            "Invalid secret specified. Please specify a valid Secret Key."
          ]
      Adw.toastOverlayAddToast (toastOverlay decodeView) toast
      pure ()
    else if secondsValid == "" || secondsValidInt < 0
           then do
             putStrLn "invalid seconds valid"
             toast <-
               Data.GI.Base.new
                 Adw.Toast
                 [ #timeout := 2
                 , #title :=
                   "Invalid seconds valid. Please specify the validity seconds window."
                 ]
             Adw.toastOverlayAddToast (toastOverlay decodeView) toast
             pure ()
           else do
             let textView = outputTextView decodeView
             textBuffer <- Gtk.textViewGetBuffer textView
             start <- Gtk.textBufferGetEndIter textBuffer
             Gtk.textBufferInsert
               textBuffer
               start
               "Starting decoding...\n\n[ "
               (-1)
             let s = encodeUtf8 secret
             let t :: Word64 = fromIntegral secondsValidInt
             let stegoParams = StegoParams s t 6 LsbEncoding 123
             decoder <- newDecoder stegoParams
             let wa = loadedAudioWave audio
             let frames = audioFrames wa
             printChan <- getResultChannel decoder
             void $
               forkIO $
               printLoop printChan (0 :: Int) (length frames) textBuffer
             decodeResult <- doDecodeFramesWithDecoder decoder frames
             threadDelay 1000000
             let stats = T.pack (show $ getResultStats decodeResult)
             iter <- Gtk.textBufferGetEndIter textBuffer
             Gtk.textBufferInsert
               textBuffer
               iter
               " ]\n\nDecoding complete.\n"
               (-1)
             iterEnd <- Gtk.textBufferGetEndIter textBuffer
             Gtk.textBufferInsert textBuffer iterEnd stats (-1)
             toast <-
               new
                 Adw.Toast
                 [#timeout := 5, #title := "Decoding completed successfully."]
             Adw.toastOverlayAddToast (toastOverlay decodeView) toast
  where
    printLoop c fs totalFs tb = do
      res <- atomically $ readTChan c
      case res of
        StoppingDecoder -> do
          pure ()
        DecodedFrameR (x, _, p) didDecode -> do
          (if didDecode
             then (do let str =
                            T.pack "<span foreground='green'>#" <>
                            T.pack (show (x + 1)) <>
                            "</span>" <> T.pack (show p)
                      iter <- Gtk.textBufferGetEndIter tb
                      Gtk.textBufferInsertMarkup tb iter str (-1)
                      printLoop c (fs + 1) totalFs tb)
             else (do let str =
                            T.pack "<span foreground='red'>#" <>
                            T.pack (show (x + 1)) <> "</span>"
                      iter <- Gtk.textBufferGetEndIter tb
                      Gtk.textBufferInsertMarkup tb iter str (-1)
                      printLoop c (fs + 1) totalFs tb))
        SkippedFrame _ -> do
          iter <- Gtk.textBufferGetEndIter tb
          let str = T.pack "<span foreground='gray'>#</span>"
          Gtk.textBufferInsertMarkup tb iter str (-1)
          printLoop c (fs + 1) totalFs tb

-- | Initialises the decodeView by loading the .ui resource and creating
-- the DecodeView data instance
initDecodeView :: AppState -> Adw.ToastOverlay -> IO DecodeView
initDecodeView appState overlay = do
  builder <- Gtk.builderNewFromResource "/gui/View/DecodeView.ui"
  encodeBin <- Gtk.builderGetObject builder "encTopBox"
  bin <- fromJust <$> Data.GI.Base.castTo Gtk.Box (fromJust encodeBin)
  -- build banner instance
  bannerPtr <- fromJust <$> Gtk.builderGetObject builder "banner"
  banner <- fromJust <$> Data.GI.Base.castTo Adw.Banner bannerPtr
  -- build Label instances
  encFilePropSrcLblPtr <-
    fromJust <$> Gtk.builderGetObject builder "encFilePropSrcLbl"
  encFilePropSrcLbl <-
    fromJust <$> Data.GI.Base.castTo Gtk.Label encFilePropSrcLblPtr
  encFilePropRateLblPtr <-
    fromJust <$> Gtk.builderGetObject builder "encFilePropRateLbl"
  encFilePropRateLbl <-
    fromJust <$> Data.GI.Base.castTo Gtk.Label encFilePropRateLblPtr
  encFilePropBitRateLblPtr <-
    fromJust <$> Gtk.builderGetObject builder "encFilePropBitRateLbl"
  encFilePropBitRateLbl <-
    fromJust <$> Data.GI.Base.castTo Gtk.Label encFilePropBitRateLblPtr
  encFilePropSamplesLblPtr <-
    fromJust <$> Gtk.builderGetObject builder "encFilePropSamplesLbl"
  encFilePropSamplesLbl <-
    fromJust <$> Data.GI.Base.castTo Gtk.Label encFilePropSamplesLblPtr
  encFilePropNumFramesLblPtr <-
    fromJust <$> Gtk.builderGetObject builder "encFilePropNumFramesLbl"
  encFilePropNumFramesLbl <-
    fromJust <$> Data.GI.Base.castTo Gtk.Label encFilePropNumFramesLblPtr
  encFilePropNumChannelsLblPtr <-
    fromJust <$> Gtk.builderGetObject builder "encFilePropNumChannelsLbl"
  encFilePropNumChannelsLbl <-
    fromJust <$> Data.GI.Base.castTo Gtk.Label encFilePropNumChannelsLblPtr
  -- build EntryRow instances
  secretKeyEntryRowPtr <-
    fromJust <$> Gtk.builderGetObject builder "secretKeyEntryRow"
  secretKeyEntryRow <-
    fromJust <$> Data.GI.Base.castTo Adw.EntryRow secretKeyEntryRowPtr
  secondsValidEntryRowPtr <-
    fromJust <$> Gtk.builderGetObject builder "secondsValidEntryRow"
  secondsValidEntryRow <-
    fromJust <$> Data.GI.Base.castTo Adw.EntryRow secondsValidEntryRowPtr
  -- build button instances
  runDecoderBtnPtr <- fromJust <$> Gtk.builderGetObject builder "runDecoderBtn"
  runDecoderBtn <- fromJust <$> Data.GI.Base.castTo Gtk.Button runDecoderBtnPtr
  -- frames scrolled Window
  framesScrolledWindowPtr <-
    fromJust <$> Gtk.builderGetObject builder "scrolled_window"
  framesScrolledWindow <-
    fromJust <$> Data.GI.Base.castTo Gtk.ScrolledWindow framesScrolledWindowPtr
  framesBoxPtr <- fromJust <$> Gtk.builderGetObject builder "frames_container"
  framesBox <- fromJust <$> Data.GI.Base.castTo Gtk.Box framesBoxPtr
  -- output textview
  outputTextViewPtr <-
    fromJust <$> Gtk.builderGetObject builder "output_textview"
  outputTextView <-
    fromJust <$> Data.GI.Base.castTo Gtk.TextView outputTextViewPtr
  -- build eventView state ADT
  let ev =
        DecodeView
          "Decode"
          "decodeView"
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
          runDecoderBtn
          framesScrolledWindow
          framesBox
          overlay
          outputTextView
  -- | register event handlers
  Adw.after runDecoderBtn #clicked $ do onDecodeBtnClicked appState ev
  pure ev
