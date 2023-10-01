module View.EncodeView (
  EncodeView (..),
  initEncodeView,
)
where

import Data.GI.Base
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Adw (AttrOp ((:=)), new)
import GI.Adw qualified as Adw
import GI.GObject qualified as GObject
import GI.Gtk qualified as Gtk

data EncodeView = EncodeView
  { title :: T.Text
  , id :: T.Text
  , encodeViewBox :: Gtk.Box
  }

initEncodeView :: IO EncodeView
initEncodeView = do
  builder <- Gtk.builderNewFromResource "/gui/View/EncodeView.ui"
  encodeBin <- Gtk.builderGetObject builder "encFileTopBox"
  bin <- castTo Gtk.Box (fromJust encodeBin)
  pure $ EncodeView "Encode" "encodeView" (fromJust bin)
