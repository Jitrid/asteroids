{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import Common

-- | The current state of the game.
-- Is composed of the active entities and game statistics.
data GameState = Play {
    ship      :: Ship,
    asteroids :: [Asteroid],
    enemies   :: [Enemy],
    bullets   :: [Bullet],
    score     :: Int,
    lives     :: Int,
    level     :: Int,
    time      :: Float,
    paused    :: Bool
} | GameOver

initialState :: GameState
-- initialState = GameOver
initialState = Play {
    ship      = Ship { pos = (0, 0), dir = (0, 0), spd = (0, 0), hbx = (10, 10) },
    asteroids = [],
    enemies   = [],
    bullets   = [],
    score     = 0,
    lives     = 3,
    level     = 1,
    time      = 0,
    paused    = False
}
