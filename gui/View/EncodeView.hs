module View.EncodeView
  ( EncodeView(..)
  , initEncodeView
  , updateEncodeViewAudioFileLoaded
  ) where

import AppState (AppState(loadedAudio), AppStateLoadedAudio(loadedAudioWave))
import Command.EncodeCmd (doEncodeFramesWithEncoder)
import Control.Concurrent
  ( MVar
  , forkIO
  , newEmptyMVar
  , putMVar
  , readMVar
  , tryTakeMVar
  )
import Control.Concurrent.STM (atomically, readTChan)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.Audio.Wave (WaveAudio(..), waveAudioToFile)
import Data.GI.Base (AttrOp((:=)), castTo, new)
import Data.List (sort)
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
import Stego.Decode.Decoder (getResultFrames, getResultStats)
import Stego.Encode.Encoder
  ( EncoderResult(EncodedFrame, SkippedFrame, StoppingEncoder)
  , getResultChannel
  , newEncoder
  )

data EncodeView =
  EncodeView
    { title :: T.Text
    , id :: T.Text
    , appState :: AppState
    , encodeViewBox :: !Gtk.Box
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
    , runEncoderBtn :: !Gtk.Button
    , framesWindow :: !Gtk.ScrolledWindow
    , framesContainer :: !Gtk.Box
    , toastOverlay :: !Adw.ToastOverlay
    , outputTextView :: !Gtk.TextView
    , encodeDestinationBtnContent :: !Adw.ButtonContent
    , encodedWaveAudio :: MVar WaveAudio
    , writeEncoderFileBtn :: !Gtk.Button
    }

-- | Updates the EncodeView listbox with the properties of the loaded
-- newly audio file
updateEncodeViewAudioFileLoaded :: AppState -> EncodeView -> IO ()
updateEncodeViewAudioFileLoaded appState encodeView = do
  audio <- readMVar $ loadedAudio appState
  let wa = loadedAudioWave audio
  let srcFilePath = T.pack (srcFile wa)
  let srcLbl = encFilePropSrcLbl encodeView
  Gtk.labelSetLabel srcLbl srcFilePath
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
  let fileExt = last (T.splitOn "/" srcFilePath)
  print fileExt
  let outputFile = T.splitOn "." fileExt
  print outputFile
  Adw.setButtonContentLabel
    (encodeDestinationBtnContent encodeView)
    (head outputFile <> "_enc.wav")
  Adw.bannerSetRevealed banner False
  -- clear the output textview
  let textView = outputTextView encodeView
  textBuffer <- Gtk.textViewGetBuffer textView
  startIter <- Gtk.textBufferGetStartIter textBuffer
  endIter <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferDelete textBuffer startIter endIter
  let runEncodeBtn = runEncoderBtn encodeView
  let framesBox = framesContainer encodeView
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
  Gtk.setWidgetSensitive runEncodeBtn True

-- | Handler for the onClicked event of the RunEncoderBtn
onEncodeBtnClicked :: AppState -> EncodeView -> IO ()
onEncodeBtnClicked appState encodeView = do
  putStrLn "encode btn clicked"
  audio <- readMVar $ loadedAudio appState
  let secretKey = secretKeyEntryRow encodeView
  secret <- Gtk.editableGetText secretKey
  secondsValid <- Gtk.editableGetText (secondsValidEntryRow encodeView)
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
      Adw.toastOverlayAddToast (toastOverlay encodeView) toast
      pure ()
    else if secondsValid == "" || secondsValidInt < 1
           then do
             putStrLn "invalid seconds valid"
             toast <-
               Data.GI.Base.new
                 Adw.Toast
                 [ #timeout := 2
                 , #title :=
                   "Invalid seconds valid. Please specify the validity seconds window."
                 ]
             Adw.toastOverlayAddToast (toastOverlay encodeView) toast
             pure ()
           else do
             putStrLn "valid encoding params"
             let textView = outputTextView encodeView
             textBuffer <- Gtk.textViewGetBuffer textView
             start <- Gtk.textBufferGetEndIter textBuffer
             Gtk.textBufferInsert
               textBuffer
               start
               "Starting encoding...\n\n[ "
               (-1)
             let s = encodeUtf8 secret
             let t :: Word64 = fromIntegral secondsValidInt
             let stegoParams = StegoParams s t 6 LsbEncoding 123 False
             encoder <- newEncoder stegoParams
             let wa = loadedAudioWave audio
             let frames = audioFrames wa
             printChan <- getResultChannel encoder
             void $
               forkIO $
               printLoop printChan (0 :: Int) (length frames) textBuffer
             encodeResult <- doEncodeFramesWithEncoder encoder frames
             let stats = T.pack (show $ getResultStats encodeResult)
             iter <- Gtk.textBufferGetEndIter textBuffer
             Gtk.textBufferInsert
               textBuffer
               iter
               " ]\n\nEncoding complete.\n"
               (-1)
             iterEnd <- Gtk.textBufferGetEndIter textBuffer
             Gtk.textBufferInsert textBuffer iterEnd stats (-1)
             targetFile <-
               Adw.getButtonContentLabel
                 (encodeDestinationBtnContent encodeView)
             let encodedWa =
                   WaveAudio
                     { srcFile = T.unpack targetFile
                     , bitSize = bitSize wa
                     , rate = rate wa
                     , channels = channels wa
                     , audioFrames = []
                     , samples =
                         concatMap snd (sort $ getResultFrames encodeResult)
                     }
             _ <- tryTakeMVar (encodedWaveAudio encodeView)
             putMVar (encodedWaveAudio encodeView) encodedWa
             toast <-
               new
                 Adw.Toast
                 [#timeout := 5, #title := "Encoding completed successfully."]
             Adw.toastOverlayAddToast (toastOverlay encodeView) toast
             let writeBtn = writeEncoderFileBtn encodeView
             Gtk.setWidgetSensitive writeBtn True
  where
    printLoop c fs totalFs tb = do
      res <- atomically $ readTChan c
      case res of
        StoppingEncoder -> do
          pure ()
        EncodedFrame _ -> do
          iter <- Gtk.textBufferGetEndIter tb
          let str = T.pack "<span foreground='green'>#</span>"
          Gtk.textBufferInsertMarkup tb iter str (-1)
          printLoop c (fs + 1) totalFs tb
        SkippedFrame _ -> do
          iter <- Gtk.textBufferGetEndIter tb
          let str = T.pack "<span foreground='gray'>#</span>"
          Gtk.textBufferInsertMarkup tb iter str (-1)
          printLoop c (fs + 1) totalFs tb

onWriteEncodeFileBtnClicked :: EncodeView -> IO ()
onWriteEncodeFileBtnClicked encodeView = do
  putStrLn "Write encode clicked"
  wa <- tryTakeMVar (encodedWaveAudio encodeView)
  case wa of
    Nothing -> do
      toast <-
        new
          Adw.Toast
          [ #timeout := 3
          , #title := "Failed to write encoded audio, no encoded audio loaded."
          ]
      Adw.toastOverlayAddToast (toastOverlay encodeView) toast
    Just wa' -> do
      let textView = outputTextView encodeView
      textBuffer <- Gtk.textViewGetBuffer textView
      iterEndWrite <- Gtk.textBufferGetEndIter textBuffer
      let str =
            T.pack $ "Writing encoded audio to file " <> srcFile wa' <> "...\n"
      Gtk.textBufferInsert textBuffer iterEndWrite str (-1)
      write <- runExceptT (waveAudioToFile (srcFile wa') wa')
      case write of
        Left err -> putStrLn err
        Right _ -> do
          iterEndWrite' <- Gtk.textBufferGetEndIter textBuffer
          let str' =
                T.pack $
                "Successfully wrote encoded audio to " <> srcFile wa' <> ".\n"
          Gtk.textBufferInsert textBuffer iterEndWrite' str' (-1)
          Gtk.setWidgetSensitive (writeEncoderFileBtn encodeView) False

-- | Initialises the encodeView by loading the .ui resource and creating
-- the EncodeView data instance
initEncodeView :: AppState -> Adw.ToastOverlay -> IO EncodeView
initEncodeView appState overlay = do
  builder <- Gtk.builderNewFromResource "/gui/View/EncodeView.ui"
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
  runEncoderBtnPtr <- fromJust <$> Gtk.builderGetObject builder "runEncoderBtn"
  runEncoderBtn <- fromJust <$> Data.GI.Base.castTo Gtk.Button runEncoderBtnPtr
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
  -- encodeDestinationBtnContent
  encodeDestinationBtnContentPtr <-
    fromJust <$> Gtk.builderGetObject builder "encodeDestinationBtnContent"
  encodeDestinationBtnContent <-
    fromJust <$> castTo Adw.ButtonContent encodeDestinationBtnContentPtr
  -- Write encode File Btn
  writeEncodeFileBtnPtr <-
    fromJust <$> Gtk.builderGetObject builder "writeEncoderFileBtn"
  writeEncodeFileBtn <- fromJust <$> castTo Gtk.Button writeEncodeFileBtnPtr
  encodedWaveAudio <- newEmptyMVar
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
          framesScrolledWindow
          framesBox
          overlay
          outputTextView
          encodeDestinationBtnContent
          encodedWaveAudio
          writeEncodeFileBtn
  -- | register event handlers
  Adw.after runEncoderBtn #clicked $ do onEncodeBtnClicked appState ev
  Adw.after writeEncodeFileBtn #clicked $ do onWriteEncodeFileBtnClicked ev
  pure ev
