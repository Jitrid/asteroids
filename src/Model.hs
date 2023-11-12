{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import Common
import Graphics.Gloss

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

-- | The initial state of any new game.
initialState :: GameState
initialState = Play {
                ship      = Ship {shipCtr = (0,0), shipPos = [(-15,-15), (0,30), (15,-15), (-15,-15)], forward = False, shipDir = (0,1), shipSpd = (0, 0), shipRot = 0, shipHbx = (45, 45) },
                -- Fill the initial state a little bit.
                asteroids = [Asteroid { astPos = (100, -300), astDir = (1,1),      astSpd = (300, 220),  astHbx = (25, 25), astSize = 25 },
                             Asteroid { astPos = (-50, -200), astDir = (0.5,-0.1), astSpd = (150, -200), astHbx = (50, 50), astSize = 50 },
                             Asteroid { astPos = (-450, 350), astDir = (1,-1),     astSpd = (80,-100),   astHbx = (80, 80), astSize = 80 }],
                -- Initial state won't contain any enemies.
                enemies   = [],
                bullets   = [],
                score     = 0,
                lives     = 3,
                level     = 1,
                time      = 0,
                paused    = False
              }

splitAsteroid :: Asteroid -> [Asteroid]
splitAsteroid ast
  | astSize ast > 40 = [ast { astSize = newSize, astPos = (x + offset, y) },
                        ast { astSize = newSize, astPos = (x - offset, y) }]
  | otherwise        = []
    where
        newSize = astSize ast / 2
        (x, y)  = astPos ast
        offset  = newSize / 2
