{-# LANGUAGE DuplicateRecordFields #-}

module Common where

-- Data types to assist with readability and maintainability of the code base.
type Point      = (Float,Float)
type Direction  = (Float,Float)
type Velocity   = (Float,Float)
type HitboxUnit = (Float,Float)
data Difficulty = Easy | Normal | Hard | Extreme deriving (Eq, Show)

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
