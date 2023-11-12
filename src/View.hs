-- | This module defines how to turn the game state into a picture.
module View where

import Model
import Common
import Graphics.Gloss

renderGame :: GameState -> IO Picture
renderGame = return . viewPure

viewPure :: GameState -> Picture
viewPure GameOver = translate (-724 / 2) 0 (color red (text "Game Over!"))
viewPure gstate
  | paused gstate = translate (-1540 / 4) 0 (scale 0.5 0.5 (color white (text "Game has been paused")))
  | otherwise = pictures [pictures (map draw (asteroids gstate)),
                          pictures (map draw (enemies gstate)),
                          pictures (map draw (bullets gstate)),
                          renderFlame (ship gstate) (time gstate),
                          color white (text (show (lives gstate))),
                          color red (text (show (score gstate))),
                          draw (ship gstate)]
