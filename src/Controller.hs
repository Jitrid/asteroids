{-# LANGUAGE UnicodeSyntax #-}

-- | This module handles time and user input.
module Controller where

import Model
import Common
import Graphics.Gloss.Interface.IO.Game

adjust :: (Int -> Int -> Int) -> GameState -> GameState
adjust f g
  | lives g' >= 3 = g' { lives = 3 }
  | lives g' <= 0 = GameOver
  | otherwise     = g'
  where g' = g { lives = f (lives g) 1 }

handleEvents :: Event -> GameState -> GameState
-- handleEvents event gs = traceShow event gs
-- starting a new game
handleEvents (EventKey (Char 'N') Down (Modifiers Down Up Up) _) _ = initialState
-- stop game from crashing when pressing keys while game over
handleEvents _ GameOver = GameOver
-- pausing the game
handleEvents (EventKey (SpecialKey s) Down _ _) gstate
    | paused gstate = pause False
    | otherwise     = pause True
        where 
            pause :: Bool -> GameState
            pause b = if s == KeyEsc then gstate { paused = b } else gstate
-- mouse input
handleEvents (EventKey (MouseButton m) Down _ _) gstate
    | paused gstate = gstate -- freeze game when paused
    | otherwise = case m of 
        LeftButton  -> adjust (+) gstate
        RightButton -> adjust (-) gstate
        _           -> gstate
-- keyboard input
handleEvents (EventKey (Char c) Down _ _) gstate
    | paused gstate = gstate
    | otherwise = case c of 
        'w' -> up
        's' -> down
        'a' -> gstate {
            ship = Common.rotate 0.2 (ship gstate)
        } -- rotate left
        'd' -> gstate {
            ship = Common.rotate (-0.2) (ship gstate)
        } -- rotate right
        _   -> gstate
        where
            up   = adjust (+) gstate
            down = adjust (-) gstate
-- default
handleEvents _ gstate = gstate

handleInput :: Event -> GameState -> IO GameState
handleInput event gstate = return (handleEvents event gstate)

simulateGame :: Float -> GameState -> IO GameState
simulateGame _ GameOver = return GameOver
simulateGame sec gstate
    | paused gstate         = return gstate
    -- | time gstate + sec > 10 = return $ adjust (-) gstate { time = 0 }
    -- | otherwise             = return $ gstate { time = time gstate + sec }
    | otherwise = return gstate
