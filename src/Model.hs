{-# LANGUAGE DuplicateRecordFields #-} -- enable duplicate record fields

module Model where

import Common

-- | The data types to represent the game entities.
-- Each entity has at least a position, direction, speed, and hitbox.

-- | The ship is the player's spaceship.
-- It is the only entity that can be controlled by the player.
data Ship = Ship {
    pos :: Point,
    dir :: Direction,
    spd :: Velocity,
    hbx :: HitboxUnit
}

-- | An asteroid floats around in space, and can be destroyed by the player.
data Asteroid = Asteroid {
    pos  :: Point,
    dir  :: Direction,
    spd  :: Velocity,
    hbx  :: HitboxUnit,
    size :: Float
}

-- | An enemy is an entity that can shoot at the player.
data Enemy = Enemy {
    pos :: Point,
    dir :: Direction,
    spd :: Velocity,
    hbx :: HitboxUnit,
    dif :: Difficulty
}

-- | 
data Bullet = Bullet {
    pos :: Point, 
    dir :: Direction,
    spd :: Velocity,
    hbx :: HitboxUnit
}

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
