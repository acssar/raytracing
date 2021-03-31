module Main where

import Lib
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
deleteEv :: Event -> IO Bool
deleteEv _ = do return False

destroyEv :: IO()
destroyEv = do mainQuit

main :: IO ()
main = do
  rayfunc
  initGUI
  window <- windowNew
  onDelete window deleteEv
  onDestroy window destroyEv
  windowSetTitle window "Haskell raytracer"
  scrollWindow <- scrolledWindowNew Nothing Nothing
  containerSetBorderWidth scrollWindow 10
  image <- imageNewFromFile "result.ppm"
  scrolledWindowAddWithViewport scrollWindow image
  widgetSetSizeRequest scrollWindow 800 600

  controlPanel <- vBoxNew False 2
  containerSetBorderWidth controlPanel 10

  entry <- entryNew
  entrySetWidthChars entry 20

  hBox <- hBoxNew False 2
  boxPackStart hBox scrollWindow PackNatural 0
  boxPackStart hBox controlPanel PackNatural 0
  containerAdd window hBox
  --boxPackStart controlPanel entry PackNatural 0

  widgetShowAll window

  mainGUI
