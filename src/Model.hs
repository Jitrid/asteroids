{-# LANGUAGE DuplicateRecordFields #-} -- enable duplicate record fields

module Model where

import Common

data Ship = Ship {
    pos :: Point,
    dir :: Direction,
    spd :: Velocity,
    hbx :: HitboxUnit
}

data Asteroid = Asteroid {
    pos  :: Point,
    dir  :: Direction,
    spd  :: Velocity,
    hbx  :: HitboxUnit,
    size :: Float
}

data Enemy = Enemy {
    pos :: Point,
    dir :: Direction,
    spd :: Velocity,
    hbx :: HitboxUnit,
    dif :: Difficulty
}

data Bullet = Bullet {
    pos :: Point, 
    dir :: Direction,
    spd :: Velocity,
    hbx :: HitboxUnit
}

-- | Represents the current state of the game.
-- Is composed of the ship, asteroids, enemies, bullets, score, lives and level.
data GameState = Play {
    ship      :: Ship,
    asteroids :: [Asteroid],
    enemies   :: [Enemy],
    bullets   :: [Bullet],
    score     :: Int,
    lives     :: Int,
    level     :: Int
} | GameOver

initialState :: GameState
initialState = Play {
    ship      = Ship { pos = (0, 0), dir = (0, 0), spd = (0, 0), hbx = (10, 10) },
    asteroids = [],
    enemies   = [],
    bullets   = [],
    score     = 0,
    lives     = 3,
    level     = 1
}
