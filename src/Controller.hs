{-# LANGUAGE UnicodeSyntax #-}

-- | This module handles time and user input.
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

adjust :: (Int -> Int -> Int) -> GameState -> GameState
adjust f g
  | lives g' >= 3 = g' { lives = 3 }
  | lives g' <= 0 = GameOver
  | otherwise     = g'
  where g' = g { lives = f (lives g) 1 }

handleEvents :: Event -> GameState -> GameState
-- pausing the game
handleEvents (EventKey (SpecialKey s) Down _ _) gstate
    | paused gstate = pause False
    | otherwise     = pause True
        where 
            pause :: Bool -> GameState
            pause b = if s == KeyEsc then gstate { paused = b } else gstate
-- mouse inpit
handleEvents (EventKey (MouseButton m) Down _ _) gstate
    | paused gstate = gstate
    | otherwise = case m of 
        LeftButton  -> adjust (+) gstate
        RightButton -> adjust (-) gstate
-- keyboard input
handleEvents (EventKey (Char c) Down _ _) gstate
    | paused gstate = gstate
    | otherwise = case c of 
        'w' -> up
        's' -> down
        'a' -> down
        'd' -> up
        'p' -> gstate { paused = True }
        where
            up   = adjust (+) gstate
            down = adjust (-) gstate
-- default
handleEvents _ gstate = gstate

simulate :: Float -> GameState -> GameState
simulate _ GameOver = GameOver
simulate sec gstate
    | paused gstate         = gstate
    | time gstate + sec > 2 = adjust (-) gstate { time = 0 }
    | otherwise             = gstate { time = time gstate + sec }

handleInput :: Event -> GameState -> IO GameState
handleInput event gstate = return (handleEvents event gstate)

simulateGame :: Float -> GameState -> IO GameState
simulateGame _ GameOver = return GameOver
simulateGame sec gstate
    | paused gstate         = return gstate
    | time gstate + sec > 5 = return $ adjust (-) gstate { time = 0 }
    | otherwise             = return $ gstate { time = time gstate + sec }
