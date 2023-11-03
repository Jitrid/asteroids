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
initialState = Play {
    ship      = Ship { shipPos = [(-25,-25), (0,50), (25,-25), (-25,-25)], shipDir = (0, 0), shipSpd = (0, 0), shipRot = 0, shipHbx = (10, 10) },
    asteroids = [],
    enemies   = [],
    bullets   = [],
    score     = 0,
    lives     = 3,
    level     = 1,
    time      = 0,
    paused    = False
}
