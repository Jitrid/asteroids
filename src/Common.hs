{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    shipCtr :: Point,   
    shipPos :: Path,
    forward :: Bool,
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

-- | Draw functions

class Drawable a where
    draw :: a -> Picture

instance Drawable Ship where
    -- draw :: Ship -> Picture
    draw ship = translate locX locY $ Rotate rotationAngleDegree $ color white $ line path
        where
            (locX, locY) = shipCtr ship
            (dx, dy)     = shipDir ship
            rotationAngle = atan2 dx dy
            rotationAngleDegree = rotationAngle * 180 / pi
            rotatedPath  = map (rotatePoint rotationAngle (0,0)) (shipPos ship)
            path =  shipPos ship

instance Drawable Asteroid where
    -- draw :: Asteroid -> Picture
    draw asteroid = uncurry translate (astPos asteroid) $ color green $ circleSolid (astSize asteroid)

instance Drawable Enemy where
    -- draw :: Enemy -> Picture
    draw enemy = uncurry translate (enemyPos enemy) $ color blue $ circleSolid 20

instance Drawable Bullet where
    -- draw :: Bullet -> Picture
    draw bullet = uncurry translate (bulletPos bullet) $ color red $ circleSolid 2.5

-- | Move function

class Move a where
    move :: Float -> a -> a

instance Move Ship where
    -- move :: Float -> Ship -> Ship
    move deltaTime ship = ship {
            shipCtr = newLocation,
            shipPos = shipPos ship
        } 
        where
            (locX, locY) = shipCtr ship
            (dx, dy)     = shipSpd ship
            newLocation  = (locX + dx * deltaTime, locY + dy * deltaTime)

instance Move Asteroid where
    -- move :: Float -> Asteroid -> Asteroid
    move deltaTime asteroid = asteroid { astPos = (newX, newY) }
        where
            (currentX, currentY) = astPos asteroid
            (velocityX, velocityY) = astSpd asteroid

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

-- | Player Rotation

class Rotation a where
    rotate :: Float -> a -> a

instance Rotation Ship where
    -- rotate :: Float -> Ship -> Ship
    rotate dt ship = ship { shipDir = if shipRot ship /= 0
                                      then (cosR * dx - sinR * dy, sinR * dx + cosR * dy)
                                      else shipDir ship 
                                    }
        where
            rotationAmount = shipRot ship * dt
            cosR = cos rotationAmount
            sinR = sin rotationAmount
            (dx, dy) = shipDir ship
            rotationSpeed = 1.05
            angleChange = rotationSpeed * dt

-- | Miscellaneous

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint angle (cx, cy) (x, y) =
    let x' = cx + (x - cx) * cos angle - (y - cy) * sin angle
        y' = cy + (x - cx) * sin angle + (y - cy) * cos angle
    in (x', y')

translatePoint :: Point -> Point -> Point
translatePoint (cx, cy) (x, y) = (x + cx, y + cy)

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

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isOutOfBounds :: Point -> Bool
isOutOfBounds (x, y) = x < -(fst screenSize) || x > fst screenSize || y < -(snd screenSize) || y > snd screenSize
