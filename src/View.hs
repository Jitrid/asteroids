module View where

import Model
import Common

import Data.List
import Graphics.Gloss

renderGame :: GameState -> IO Picture
renderGame GameOver = gameOver
renderGame gstate
  | paused gstate = return $ translate (-1540 / 4) 0 (scale 0.5 0.5 (color white (text "Game has been paused")))
  | otherwise = return $ pictures [pictures (map draw (asteroids gstate)),
                                   pictures (map draw (enemies gstate)),
                                   pictures (map draw (bullets gstate)),
                                   draw (ship gstate),                 -- player
                                   render (time gstate) (ship gstate), -- animation
                                   -- HUD
                                   scale 0.3 0.3 $ translate (-1850) 950 $ color white (text ("Lives: " ++ show (lives gstate))),
                                   scale 0.3 0.3 $ translate (-1850) 750 $ color white (text ("Score: " ++ show (score gstate)))]

-- | Read high scores from file and display them.

gameOver :: IO Picture
gameOver = do
    highscores <- getHighScores
    return $ pictures [scale 0.5 0.5 $ translate (-724 / 2) 250 (color red (text "Game Over!")),
                       translate (-1540 / 4) 0 (scale 0.5 0.5 (color white (text "Highscores:"))),
                       translate (-1540 / 4) (-100) (scale 0.25 0.25 (color (makeColor 1 0.84 0 0) (text (highscores !! 0)))),
                       translate (-1540 / 4) (-150) (scale 0.25 0.25 (color (makeColor 0.90 0.90 0.90 0) (text (highscores !! 1)))),
                       translate (-1540 / 4) (-200) (scale 0.25 0.25 (color (makeColor 0.80 0.50 0.20 0) (text (highscores !! 2))))]

getHighScores :: IO [String]
getHighScores = do
    contents <- readFile "scores.txt"
    let sortedScores = take 3 $ sortOn (negate . read) (lines contents)
    return sortedScores
