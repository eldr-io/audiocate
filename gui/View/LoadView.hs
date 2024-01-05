-- | Module that defines the LoadView
module View.LoadView
  ( LoadView (..),
    initLoadView,
  )
where

import AppState (AppState (..), AppStateLoadedAudio (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar, tryTakeMVar)
import Control.Monad.Except (runExceptT)
import Data.Audio.Wave (WaveAudio, waveAudioFromFile)
import Data.GI.Base (castTo)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Adw (AttrOp ((:=)), new, toastOverlayAddToast)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

-- | Represents the state of the LoadView instance
data LoadView = LoadView
  { title :: T.Text,
    id :: T.Text,
    loadedFilePath :: MVar FilePath,
    loadViewBox :: !Gtk.Box,
    loadFileSelectedLbl :: !Gtk.Label
  }

-- | Handles the async callback from the audio file loading FileDialog
-- which sets the loaded filepath state by storing the filepath on the LoadView
handleSelectFileChosen :: LoadView -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleSelectFileChosen lv fileDialog ptrData = do
  putStrLn "handle called"
  file <- Gtk.fileDialogOpenFinish fileDialog ptrData
  case file of
    Nothing -> do
      putStrLn "No file selected."
      pure ()
    Just gFile -> do
      name <- Gio.fileGetBasename gFile
      path <- Gio.fileGetPath gFile
      putStrLn $ fromJust name
      putStrLn $ fromJust path
      _ <- tryTakeMVar (loadedFilePath lv)
      putMVar (loadedFilePath lv) (fromJust path)
      let lbl = loadFileSelectedLbl lv
      Gtk.labelSetLabel lbl (T.pack "<i>" <> T.pack (fromJust name) <> "</i>")

-- | Handles the click of the loadFileBtn which attempts to load the
-- audio file using the audiocate lib
handleLoadFileBtnClicked :: LoadView -> IO (Either String WaveAudio)
handleLoadFileBtnClicked lv = do
  filePath <- tryReadMVar (loadedFilePath lv)
  case filePath of
    Nothing -> do
      pure $ Left "No file selected"
    Just path -> do
      runExceptT $ waveAudioFromFile path

-- | Initialises the Load view and returns the state object
initLoadView :: Adw.ApplicationWindow -> AppState -> Adw.ToastOverlay -> MVar Bool -> IO LoadView
initLoadView window state overlay fileLoadMVar = do
  builder <- Gtk.builderNewFromResource "/gui/View/LoadView.ui"
  loadViewBox <- Gtk.builderGetObject builder "loadFileTopBox"
  box <- fromJust <$> castTo Gtk.Box (fromJust loadViewBox)

  selectFileBtnObj <- Gtk.builderGetObject builder "selectFileBtn"
  selectFileBtn <- fromJust <$> castTo Adw.SplitButton (fromJust selectFileBtnObj)
  loadFileBtnObj <- Gtk.builderGetObject builder "loadFileBtn"
  loadFileBtn <- fromJust <$> castTo Gtk.Button (fromJust loadFileBtnObj)

  loadFileSelectedLblObj <- fromJust <$> Gtk.builderGetObject builder "loadFileSelectedLbl"
  loadFileSelectedLbl <- fromJust <$> castTo Gtk.Label loadFileSelectedLblObj

  loadedFilePathMVar <- newEmptyMVar

  let lv = LoadView "Load" "loadView" loadedFilePathMVar box loadFileSelectedLbl

  -- attach eventhandlers for the view
  Adw.after selectFileBtn #clicked $ do
    cancelCtx <- Gio.cancellableNew
    fileDialog <- new Gtk.FileDialog [#title := "Load a .WAV file into Audiocate"]
    Gtk.fileDialogOpen
      fileDialog
      (Just window)
      (Just cancelCtx)
      ( Just
          ( \_ ptrData -> do
              fileResult <- fromJust <$> castTo Gtk.FileDialog fileDialog
              handleSelectFileChosen lv fileResult ptrData
          )
      )

  Adw.after loadFileBtn #clicked $ do
    audio <- handleLoadFileBtnClicked lv
    case audio of
      Left err -> do
        toast <-
          new
            Adw.Toast
            [ #timeout := 2,
              #title := T.pack ("Failed to load audio file " <> err)
            ]
        toastOverlayAddToast overlay toast
      Right wa -> do
        _ <- tryTakeMVar (loadedAudio state)
        filePath <- readMVar (loadedFilePath lv)
        let la = AppStateLoadedAudio wa True filePath
        putMVar (loadedAudio state) la
        toast <-
          new
            Adw.Toast
            [ #timeout := 1,
              #title := T.pack ("Successfully loaded audio file " <> filePath)
            ]
        toastOverlayAddToast overlay toast
        putStrLn "Setting fileLoadMVar"
        putMVar fileLoadMVar True

  pure lv
