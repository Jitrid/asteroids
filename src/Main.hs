module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

import Graphics.UI.GLUT (getArgsAndInitialize)

main :: IO ()
main = do --getArgsAndInitialize
          --centerText (Text "Alive(x)")
          --centerText (Text "Game Over!")
          playIO (InWindow "Asteroids on Steroids" (1200, 700) (0, 0))
              black
              24
              initialState
              renderGame       -- View function
              handleInput      -- Event function
              simulateGame     -- Step function
