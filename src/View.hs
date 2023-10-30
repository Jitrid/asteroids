-- | This module defines how to turn the game state into a picture.
module View where

import Model
import Graphics.Gloss

import Graphics.UI.GLUT.Fonts

renderGame :: GameState -> IO Picture
renderGame = pure . viewPure

-- temp function to determine width of a text.
centerText :: Picture -> IO ()
centerText (Text str) = do
    width <- stringWidth Roman str
    putStrLn $ "Width: " ++ show (fromIntegral width)

viewPure :: GameState -> Picture
viewPure GameOver = translate (-724 / 2) 0 (color red (text "Game Over!"))
viewPure gstate
  | paused gstate = translate (-1540 / 4) 0 (scale 0.5 0.5 (color white (text "Game has been paused")))
  -- | otherwise = pictures [ draw (ship gstate) ]
  | otherwise     = case compare (lives gstate) 0 of
        GT -> translate (-405 / 2) 0 (color green (text ("Alive (" ++ show (lives gstate) ++ ")")))
    --   3 -> color green (translate (-705 / 2) 0 (Text "Hello World!"))
