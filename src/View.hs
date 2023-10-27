-- | The module to define how to turn the game state into a picture.
module View where

import Model
import Common
import Graphics.Gloss

renderGame :: GameState -> IO Picture
renderGame = pure . viewPure

viewPure :: GameState -> Picture
viewPure GameOver = color red (text "Game Over!")
viewPure gstate
  | paused gstate = translate (-500) 0 (scale 0.5 1 (color white (text "Game has been paused")))
  | otherwise = pictures [ draw (ship gstate) ]
  -- | otherwise     = case lives gstate of
  --     1 -> color green (text "1") -- ♡
  --     2 -> color green (text "2")
  --     3 -> color green (text "3")
