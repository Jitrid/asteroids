-- | The module to define how to turn the game state into a picture.
module View where

import Model
import Graphics.Gloss

renderGame :: GameState -> IO Picture
renderGame = pure . viewPure

viewPure :: GameState -> Picture
viewPure GameOver = color red (text (show "Game Over!"))
viewPure (Play _ _ _ _ _ lives _ _) = case lives of
  1 -> color green (text (show "1")) -- ♡
  2 -> color green (text (show "2"))
  3 -> color green (text (show "3"))
