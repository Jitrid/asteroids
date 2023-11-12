module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Common (screenSize)

main :: IO ()
main = do playIO (InWindow "Asteroids on Steroids" (1200, 700) (0, 0))
              black
              24
              initialState
              renderGame       -- View function
              handleInput      -- Event function
              simulateGame     -- Step function
