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
    movingForward :: Bool,
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
    enemyFireCD :: Float,
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

-- instance Rotation Ship where
--     rotate r s = s {
--         shipPos = map (rotate' r) (shipPos s)
--     }
--         where
--             rotate' :: Float -> Point -> Point
--             rotate' r' (x,y) = (x * cos r' - y * sin r', x * sin r' + y * cos r')


instance Rotation Ship where
    -- rotate dt s = 
    --     let currentRot = shipRot s 
    --         newRot = currentRot + 0.1 * dt 
    --     in s {shipRot = newRot}




    rotate r s = s {
         shipPos = map (rotateAroundCenter r (shipCenter (shipPos s))) (shipPos s)
     }
        where
             rotateAroundCenter :: Float -> Point -> Point -> Point
             rotateAroundCenter r' center (x, y) = 
                let 
                 -- Translate point to origin
                (x', y') = (x - cx, y - cy)
                cx = fst center
                cy = snd center

                -- Rotate point
                (xRot, yRot) = (x' * cos r' - y' * sin r', x' * sin r' + y' * cos r')

               -- Translate point back
                in (xRot + cx, yRot + cy)


shipCenter :: Path -> Point
shipCenter [xs,ys,zs,_] = 
    let n = 3
        (sumX, sumY) = foldr (\(x, y) (accX, accY) -> (accX + x, accY + y)) (0, 0) [xs, ys, zs]
    in (sumX / n, sumY / n)