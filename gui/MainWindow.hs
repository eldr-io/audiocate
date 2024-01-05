-- | MainWindow module definition, acting as the top level container 
-- of the GUI application.
module MainWindow where

import AppState (AppState)
import Audiocate (version)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forever)
import qualified Data.Text as T
import GI.Adw (AttrOp((:=)), new)
import qualified GI.Adw as Adw
import qualified GI.Gtk as Gtk
import View.DecodeView
  ( DecodeView(..)
  , initDecodeView
  , updateDecodeViewAudioFileLoaded
  )
import View.EncodeView
  ( EncodeView(..)
  , initEncodeView
  , updateEncodeViewAudioFileLoaded
  )
import View.LoadView (LoadView(..), initLoadView)
import GI.Gio (MenuItem(..), menuItemSetLabel)
import qualified GI.Gio as Gtk

-- | MainWindow ADT that holds boxed pointers to relevant components
data MainWindow =
  MainWindow
    { application :: !Adw.Application
    , appState :: AppState
    , toastOverlay :: !Adw.ToastOverlay
    , window :: !Adw.ApplicationWindow
    , encodeView :: !EncodeView
    , decodeView :: !DecodeView
    , loadView :: !LoadView
    }

-- | Sets the EncodeViewFileLoaded MVar mutex to signal that a 
-- file was successfully loaded.
updateEncodeViewFileLoad :: MVar Bool -> AppState -> EncodeView -> IO ()
updateEncodeViewFileLoad fileLoadedMVar state encodeView = do
  _ <- takeMVar fileLoadedMVar
  updateEncodeViewAudioFileLoaded state encodeView
  putMVar fileLoadedMVar True

-- | Sets the DecodeViewFileLoaded MVar mutex to signal that a 
-- file was successfully loaded.
updateDecodeViewFileLoad :: MVar Bool -> AppState -> DecodeView -> IO ()
updateDecodeViewFileLoad fileLoadedMVar state decodeView = do
  _ <- takeMVar fileLoadedMVar
  updateDecodeViewAudioFileLoaded state decodeView

-- | Initialises the MainWindow by instantiating the sub-views and 
-- the Adwaita ApplicationWindow instance.
initMainWindow :: Adw.Application -> AppState -> IO MainWindow
initMainWindow app state = do
  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  overlay <- new Adw.ToastOverlay [#child := content]
  stack <- new Adw.ViewStack [#hexpand := True]
  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #content := overlay
      , #defaultWidth := 1220
      , #defaultHeight := 800
      ]

  -- run signal handlers for fileload Mutex operations
  fileLoadedMVar <- newEmptyMVar
  _ <-
    forkIO (forever $ updateEncodeViewFileLoad fileLoadedMVar state encodeView)
  _ <-
    forkIO (forever $ updateDecodeViewFileLoad fileLoadedMVar state decodeView)

  -- instantiate sub-views
  let welcomeTitle = "Audiocate " <> version
  encodeView <- initEncodeView state overlay
  let encViewBox = encodeViewBox encodeView
  decodeView <- initDecodeView state overlay
  let decViewBox = decodeViewBox decodeView
  loadView <- initLoadView window state overlay fileLoadedMVar

  let lViewBox = loadViewBox loadView
  welcomePage <-
    new
      Adw.StatusPage
      [ #iconName := "org.gnome.Adwaita1.Demo"
      , #title := T.pack welcomeTitle
      , #description := "Audio encoding authentication and " <>
        "validation library for verifying audio as being from a trusted source"
      , #child := lViewBox
      ]
  Adw.viewStackAddTitledWithIcon
    stack
    welcomePage
    (Just "welcome-page")
    "Load"
    "audio-x-generic"
  Adw.viewStackAddTitledWithIcon
    stack
    encViewBox
    (Just "encode-page")
    "Encode"
    "sound-wave-symbolic"
  Adw.viewStackAddTitledWithIcon
    stack
    decViewBox
    (Just "decode-page")
    "Decode"
    "sound-wave-alt-symbolic"
  viewSwitcherBar <- new Adw.ViewSwitcherBar [#stack := stack]
  viewSwitcherTitle <- new Adw.ViewSwitcherTitle [#stack := stack]
  headerBar <- new Adw.HeaderBar [#titleWidget := viewSwitcherTitle]

  -- configure top left menu
  menuModelItem <- new MenuItem []
  menuItemSetLabel menuModelItem (Just $ T.pack "Toggle Light/Dark")
  menuModelItemAbout <- new MenuItem []
  menuItemSetLabel menuModelItemAbout (Just $ T.pack "About")

  menuModel <- new Gtk.Menu []
  Gtk.menuAppendItem menuModel menuModelItem
  Gtk.menuAppendItem menuModel menuModelItemAbout
  menuBtn <- new Gtk.MenuButton [ #menuModel := menuModel, #iconName := "open-menu-symbolic"]
  Adw.headerBarPackStart headerBar menuBtn
  content.append headerBar
  content.append stack
  content.append viewSwitcherBar
  let mw = MainWindow app state overlay window encodeView decodeView loadView
  pure mw
