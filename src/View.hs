-- | This module defines how to turn the game state into a picture.
module View where

import Model
import Common
import Graphics.Gloss

import Graphics.UI.GLUT.Fonts

renderGame :: GameState -> IO Picture
renderGame = viewPure

-- temp function to determine width of a text.
centerText :: Picture -> IO Float
centerText (Text str) =
    fromIntegral <$> stringWidth Roman str
    --putStrLn $ "Width: " ++ show (fromIntegral width)

getWidth :: String -> IO Float
getWidth = centerText . Text

viewPure :: GameState -> IO Picture
viewPure GameOver = do
    width <- getWidth t
    return $ translate (-width / 2) 0 (color red (text t))
    where
        t = "Game Over!"
viewPure gstate
  | paused gstate = do
    width <- getWidth t
    return $ translate (-width / 4) 0 (scale 0.5 0.5 (color white (text t)))
  | otherwise = return (pictures [ draw (ship gstate) ])
--   | otherwise     = case compare (lives gstate) 0 of
--         GT -> do
--             width <- getWidth t'
--             return $ translate (-width / 2) 0 (color green (text t'))
    where
        t  = "Game has been paused"
        t' = "Alive (" ++ show (lives gstate) ++ ")"

