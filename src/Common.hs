{-# LANGUAGE DuplicateRecordFields #-}

module Common where

import Graphics.Gloss

-- Data types to assist with readability and maintainability of the code base.
type Direction  = (Float,Float)
type Velocity   = (Float,Float)
type HitboxUnit = (Float,Float)
data Difficulty = Easy | Normal | Hard | Extreme deriving (Eq, Show)

-- | The data types to represent the game entities.
-- Each entity has at least a position, direction, speed, and hitbox.

-- | The ship is the player's spaceship.
-- It is the only entity that can be controlled by the player.
data Ship = Ship {
    shipPos :: Path,
    shipDir :: Direction,
    shipSpd :: Velocity,
    shipRot :: Float,
    shipHbx :: HitboxUnit
}

-- | An asteroid floats around in space, and can be destroyed by the player.
data Asteroid = Asteroid {
    astPos  :: Point,
    astDir  :: Direction,
    astSpd  :: Velocity,
    astHbx  :: HitboxUnit,
    astSize :: Float
}

-- | An enemy is an entity that can shoot at the player.
data Enemy = Enemy {
    enemyPos :: Point,
    enemyDir :: Direction,
    enemySpd :: Velocity,
    enemyHbx :: HitboxUnit,
    enemyDif :: Difficulty
}

-- | 
data Bullet = Bullet {
    bulletPos :: Point,
    bulletDir :: Direction,
    bulletSpd :: Velocity,
    bulletHbx :: HitboxUnit
}

class Drawable a where
    draw :: a -> Picture

instance Drawable Ship where
    draw s = color white (line (shipPos s))

class Rotation a where
    rotate :: Float -> a -> a

instance Rotation Ship where
    rotate r s = s {
        shipPos = map (rotate' r) (shipPos s)
    }
        where
            rotate' :: Float -> Point -> Point
            rotate' r' (x,y) = (x * cos r' - y * sin r', x * sin r' + y * cos r')
