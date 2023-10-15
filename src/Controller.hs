module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

handleEvents :: Event -> GameState -> IO GameState
handleEvents _ = return

simulateGame :: Float -> GameState -> IO GameState
simulateGame _ = return

