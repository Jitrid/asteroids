{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Common where

import Graphics.Gloss

screenSize :: (Float, Float)
screenSize = (1200, 700)

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
    shipLocation :: Point,   
    shipPos      :: Path,
    movingForward :: Bool,
    shipDir      :: Direction,
    shipSpd      :: Velocity,
    shipRot      :: Float,
    shipHbx      :: HitboxUnit
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

-- | Move function

class Move a where
    move :: Float -> a -> a

instance Move Asteroid where
    -- move :: Float -> Asteroid -> Asteroid
    move deltaTime asteroid = asteroid { astPos = (newX, newY) }
        where
            (currentX, currentY) = astPos asteroid
            (velocityX, velocityY) = astSpd asteroid

            -- Calculate new position
            newX = currentX + velocityX * deltaTime
            newY = currentY + velocityY * deltaTime

instance Move Enemy where
    -- move :: Float -> Enemy -> Enemy
    move deltaTime enemy = enemy { 
            enemyPos = (newX, newY),
            enemyFireCD = newCooldown
        }
        where
            (currentX, currentY) = enemyPos enemy
            (velocityX, velocityY) = enemySpd enemy

            -- Calculate new position
            newX = currentX + velocityX * deltaTime
            newY = currentY + velocityY * deltaTime

            newCooldown = max 0 (enemyFireCD enemy - deltaTime)

instance Move Bullet where
    -- move :: Float -> Bullet -> Bullet
    move deltaTime bullet = bullet {
            bulletPos = (x + vx * speed * deltaTime, y + vy * speed * deltaTime)
        } 
        where
            (x, y) = bulletPos bullet
            speed =  fst (bulletSpd bullet)
            (vx, vy) = bulletDir bullet

-- | Update function



shipCenter :: Path -> Point
shipCenter [xs,ys,zs,_] =
    let n = 3
        (sumX, sumY) = foldr (\(x, y) (accX, accY) -> (accX + x, accY + y)) (0, 0) [xs, ys, zs]
    in (sumX / n, sumY / n)

-- | Common functions used throughout the code base.

normalize :: (Float, Float) -> (Float, Float)
normalize (dx, dy) =
    let magnitude = sqrt (dx * dx + dy * dy)
    in if magnitude == 0 then (0, 0) else (dx / magnitude, dy / magnitude)

magnitude :: Velocity -> Float
magnitude = sqrt . magnitudeSquared

magnitudeSquared :: Velocity -> Float
magnitudeSquared = dotProduct

--Vector Math
addVectors :: (Float, Float) -> (Float, Float) -> (Float, Float)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVector :: (Float, Float) -> Float -> (Float, Float)
scaleVector (x, y) s = (x * s, y * s)

rotateVector :: Float -> Direction -> Direction
rotateVector r (x, y) = (cos r * x - sin r * y, sin r * x + cos r * y)

dotProduct :: (Float, Float) -> Float
dotProduct (x, y) = x * x + y * y

-- Other

isOutOfBounds :: Point -> Bool
isOutOfBounds (x, y) = x < -(fst screenSize) || x > fst screenSize || y < -(snd screenSize) || y > snd screenSize
