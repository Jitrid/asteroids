module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Asteroids on Steroids" (640, 640) (0, 0)) -- Or FullScreen
              black            -- Background color
              24               -- Frames per second
              initialState     -- Initial state
              renderGame       -- View function
              handleEvents     -- Event function
              simulateGame     -- Step function
