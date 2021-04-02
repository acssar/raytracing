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
  --rayfunc
  initGUI

  controlPanel <- vBoxNew False 2
  containerSetBorderWidth controlPanel 10

  messageLabel <- labelNew (Just "\nDon't press this button.\n")

  coolButton <- buttonNewWithLabel "Render Button"
  --load button
  --  entry <- entryNew
  --  entrySetWidthChars entry 20

  window2 <- windowNew
  onDelete window2 deleteEv
  onDestroy window2 destroyEv
  windowSetTitle window2 "Raytrace window"
--  scrollWindow2 <- scrolledWindowNew Nothing Nothing
  hBox2 <- hBoxNew False 2
--  boxPackStart hBox2 scrollWindow2 PackNatural 0
  boxPackStart hBox2 controlPanel PackNatural 0
  boxPackStart controlPanel coolButton PackNatural 0
  containerAdd window2 hBox2
  widgetShowAll window2

  onClicked coolButton $ do
    labelSetText messageLabel "\nOh, no, you pressed the button !\n"
    rayfunc
    window <- windowNew
    --onDelete window deleteEv
    --onDestroy window destroyEv
    windowSetTitle window "Haskell raytracer"
    scrollWindow <- scrolledWindowNew Nothing Nothing
    containerSetBorderWidth scrollWindow 10
    image <- imageNewFromFile "result.ppm"
    scrolledWindowAddWithViewport scrollWindow image
    widgetSetSizeRequest scrollWindow 800 600
    hBox <- hBoxNew False 2
    boxPackStart hBox scrollWindow PackNatural 0
    containerAdd window hBox
    widgetShowAll window

  boxPackStart controlPanel messageLabel PackNatural 0

  mainGUI
