module View.LoadView (
  LoadView (..),
  initLoadView,
)
where

import Data.GI.Base (castTo)
import GI.Adw (AttrOp ((:=)), new)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Gtk qualified as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio

data LoadView = LoadView
  { title :: T.Text
  , id :: T.Text
  , loadViewBox :: !Gtk.Box
  , loadFileSelectedLbl :: !Gtk.Label
  }

-- | Handles the async callback from the audio file loading FileDialog 
-- which sets the loaded file state and enables the "Load" button
handleSelectFileChosen :: LoadView -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleSelectFileChosen lv fileDialog ptrData = do
  putStrLn "handle called"
  file <- Gtk.fileDialogOpenFinish fileDialog ptrData
  case file of
    Nothing -> pure ()
    Just gFile -> do
      name <- Gio.fileGetBasename gFile
      path <- Gio.fileGetPath gFile
      putStrLn $ fromJust name
      putStrLn $ fromJust path
      let lbl = loadFileSelectedLbl lv
      Gtk.labelSetLabel lbl (T.pack "<i>" <> T.pack (fromJust name) <> "</i>")

-- | Initialises the Load view and returns the state object
initLoadView :: Adw.ApplicationWindow -> IO LoadView
initLoadView window = do
  builder <- Gtk.builderNewFromResource "/gui/View/LoadView.ui"
  loadViewBox <- Gtk.builderGetObject builder "loadFileTopBox"
  box <- fromJust <$> castTo Gtk.Box (fromJust loadViewBox)


  selectFileBtnObj <- Gtk.builderGetObject builder "selectFileBtn"
  selectFileBtn <- fromJust <$> castTo Adw.SplitButton (fromJust selectFileBtnObj)
  loadFileBtnObj <- Gtk.builderGetObject builder "loadFileBtn"
  loadFileBtn <- fromJust <$> castTo Adw.ButtonContent (fromJust loadFileBtnObj)

  loadFileSelectedLblObj <- fromJust <$> Gtk.builderGetObject builder "loadFileSelectedLbl"
  loadFileSelectedLbl <- fromJust <$> castTo Gtk.Label loadFileSelectedLblObj

  let lv = LoadView "Load" "loadView" box loadFileSelectedLbl

  -- attach eventhandlers for the view
  Adw.after selectFileBtn #clicked $ do
    cancelCtx <- Gio.cancellableNew
    fileDialog <- new Gtk.FileDialog [#title := "Load a .WAV file into Audiocate"]
    Gtk.fileDialogOpen fileDialog (Just window) (Just cancelCtx) (
            Just (\_ ptrData -> do
              fileResult <- fromJust <$> castTo Gtk.FileDialog fileDialog
              handleSelectFileChosen lv fileResult ptrData
            ))
  pure lv
