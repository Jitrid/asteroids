module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Asteroids on Steroids" (640, 640) (0, 0))
              black
              24
              initialState
              renderGame       -- View function
              handleInput      -- Event function
              simulateGame     -- Step function
