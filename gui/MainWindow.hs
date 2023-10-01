module MainWindow where

import Audiocate (version)
import Data.Text qualified as T
import GI.Adw (AttrOp ((:=)), new)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import View.EncodeView (EncodeView (..), initEncodeView)

data MainWindow = MainWindow
  { application :: !Adw.Application,
    windowPtr :: !Adw.ApplicationWindow,
    encodeView :: !EncodeView
  }

initMainWindow :: Adw.Application -> IO MainWindow
initMainWindow app = do
  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  adjustment <-
    new
      Gtk.Adjustment
      [ #value := 50,
        #lower := 0,
        #upper := 100,
        #stepIncrement := 1
      ]
  slider <- new Gtk.Scale [#adjustment := adjustment, #drawValue := True]
  stack <- new Adw.ViewStack []

  label1 <- new Gtk.Label [#label := "Decode", #hexpand := True]
  label2 <- new Gtk.Label [#label := "Analyse", #hexpand := True]

  let welcomeTitle = "Audiocate " <> version
  loadFileBtn <- new Gtk.Button [#label := "Open file"]
  encodeView <- initEncodeView
  let encViewBox = encodeViewBox encodeView
  clamp <- new Adw.Clamp [#maximumSize := 600, #child := encViewBox]
  welcomePage <-
    new
      Adw.StatusPage
      [ #iconName := "org.gnome.Adwaita1.Demo",
        #title := T.pack welcomeTitle,
        #description := "Audio encoding authentication and "
          <> "validation library for verifying audio as being from a trusted source",
        #child := clamp
      ]

  Adw.viewStackAddTitledWithIcon stack welcomePage (Just "welcome-page") "Load" "audio-x-generic"


  Adw.viewStackAddTitledWithIcon stack loadFileBtn (Just "encode-page") "Encode" "mail-send-symbolic"
  Adw.viewStackAddTitledWithIcon stack label1 (Just "decode-page") "Decode" "mail-send-symbolic"

  viewSwitcherBar <- new Adw.ViewSwitcherBar [#stack := stack]
  viewSwitcherTitle <- new Adw.ViewSwitcherTitle [#stack := stack]
  headerBar <- new Adw.HeaderBar [#titleWidget := viewSwitcherTitle]

  menuBtn <- new Gtk.Button [#iconName := "open-menu-symbolic"]
  Adw.headerBarPackStart headerBar menuBtn

  content.append headerBar
  content.append stack
  content.append viewSwitcherBar

  window <-
    new
      Adw.ApplicationWindow
      [ #application := app,
        #content := content,
        #defaultWidth := 820,
        #defaultHeight := 550
      ]
  let mw = MainWindow app window encodeView
  pure mw
