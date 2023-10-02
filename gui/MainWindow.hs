module MainWindow where

import Audiocate (version)
import Data.Text qualified as T
import GI.Adw (AttrOp ((:=)), new)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import View.EncodeView (EncodeView (..), initEncodeView)
import View.LoadView (LoadView (..), initLoadView)

data MainWindow = MainWindow
  { application :: !Adw.Application,
    windowPtr :: !Adw.ApplicationWindow,
    encodeView :: !EncodeView
  }

initMainWindow :: Adw.Application -> IO MainWindow
initMainWindow app = do
  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  stack <- new Adw.ViewStack [#hexpand := True]

  window <-
    new
      Adw.ApplicationWindow
      [ #application := app,
        #content := content,
        #defaultWidth := 1220,
        #defaultHeight := 760
      ]

  label1 <- new Gtk.Label [#label := "Decode", #hexpand := True]

  let welcomeTitle = "Audiocate " <> version

  encodeView <- initEncodeView
  let encViewBox = encodeViewBox encodeView

  loadView <- initLoadView window
  let lViewBox = loadViewBox loadView

  welcomePage <-
    new
      Adw.StatusPage
      [ #iconName := "org.gnome.Adwaita1.Demo",
        #title := T.pack welcomeTitle,
        #description := "Audio encoding authentication and "
          <> "validation library for verifying audio as being from a trusted source",
        #child := lViewBox
      ]

  Adw.viewStackAddTitledWithIcon stack welcomePage (Just "welcome-page") "Load" "audio-x-generic"


  Adw.viewStackAddTitledWithIcon stack encViewBox (Just "encode-page") "Encode" "mail-send-symbolic"
  Adw.viewStackAddTitledWithIcon stack label1 (Just "decode-page") "Decode" "mail-send-symbolic"

  viewSwitcherBar <- new Adw.ViewSwitcherBar [#stack := stack]
  viewSwitcherTitle <- new Adw.ViewSwitcherTitle [#stack := stack]
  headerBar <- new Adw.HeaderBar [#titleWidget := viewSwitcherTitle]

  menuBtn <- new Gtk.Button [#iconName := "open-menu-symbolic"]
  Adw.headerBarPackStart headerBar menuBtn

  content.append headerBar
  content.append stack
  content.append viewSwitcherBar

  let mw = MainWindow app window encodeView
  pure mw
