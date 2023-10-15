module View where

import Model
import Graphics.Gloss

renderGame :: GameState -> IO Picture
renderGame = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case lives gstate of
  0 -> blank
  1 -> color green (text (show "Loser"))
  2 -> color green (text (show "Bueno"))
  3 -> color green (text (show "Excelente"))
