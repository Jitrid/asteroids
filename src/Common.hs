{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common where

import Graphics.Gloss

screenSize :: (Float, Float)
screenSize = (1200, 700)

-- Data types to assist with readability and maintainability of the code base.
type Direction  = (Float,Float)
type Velocity   = (Float,Float)
type HitboxUnit = (Float,Float)

-- | The data types to represent the game entities.
-- Each entity has at least a position, direction, speed, and hitbox.

-- | The ship is the player's spaceship.
-- It is the only entity that can be controlled by the player.
data Ship = Ship {
    shipCtr :: Point,   
    shipPos :: Path,
    shipDir :: Direction,
    shipSpd :: Velocity,
    shipRot :: Float,
    shipHbx :: HitboxUnit,
    forward :: Bool
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
    enemyPos    :: Point,
    enemyDir    :: Direction,
    enemySpd    :: Velocity,
    enemyHbx    :: HitboxUnit,
    enemyFireCD :: Float
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
    draw ship = translate locX locY $ Rotate rotationAngleDegree $ color white $ line (shipPos ship)
        where
            (locX, locY)        = shipCtr ship
            (dx, dy)            = shipDir ship
            rotationAngle       = atan2 dx dy
            rotationAngleDegree = rotationAngle * 180 / pi
            rotatedPath         = map (rotatePoint rotationAngle (0,0)) (shipPos ship)

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

class Moveable a where
    move :: Float -> a -> a

instance Moveable Ship where
    -- move :: Float -> Ship -> Ship
    move deltaTime ship = ship {
            shipCtr = wrappedLocation,
            shipPos = shipPos ship
        }
        where
            (locX, locY) = shipCtr ship
            (dx, dy)     = shipSpd ship
            newLocation  = (locX + dx * deltaTime, locY + dy * deltaTime)
            wrappedLocation = wrapPoint newLocation

instance Moveable Asteroid where
    -- move :: Float -> Asteroid -> Asteroid
    move deltaTime asteroid = asteroid { astPos = (newX, newY) }
        where
            (currentX, currentY)   = astPos asteroid
            (velocityX, velocityY) = astSpd asteroid

            newX = currentX + velocityX * deltaTime
            newY = currentY + velocityY * deltaTime

instance Moveable Enemy where
    -- move :: Float -> Enemy -> Enemy
    move deltaTime enemy = enemy { 
            enemyPos    = (newX, newY),
            enemyFireCD = newCooldown
        }
        where
            (currentX, currentY)   = enemyPos enemy
            (velocityX, velocityY) = enemySpd enemy

            newX = currentX + velocityX * deltaTime
            newY = currentY + velocityY * deltaTime

            newCooldown = max 0 (enemyFireCD enemy - deltaTime)

instance Moveable Bullet where
    -- move :: Float -> Bullet -> Bullet
    move deltaTime bullet = bullet {
            bulletPos = (x + vx * speed * deltaTime, y + vy * speed * deltaTime)
        } 
        where
            (x, y)   = bulletPos bullet
            speed    = fst (bulletSpd bullet)
            (vx, vy) = bulletDir bullet

-- | Player Rotation

class Rotation a where
    rotate :: Float -> a -> a

instance Rotation Ship where
    -- rotate :: Float -> Ship -> Ship
    rotate dt ship = ship { shipDir = if shipRot ship /= 0 then (cosR * dx - sinR * dy, sinR * dx + cosR * dy)
                                      else shipDir ship 
                                    }
        where
            rotationAmount = shipRot ship * dt
            cosR = cos rotationAmount
            sinR = sin rotationAmount
            (dx, dy) = shipDir ship

-- | Animations

class Animatable a where
    animate :: a -> [Point]
    render :: Float -> a -> Picture

instance Animatable Ship where
    -- animate :: Ship -> [Point]
    animate s = let center = shipCtr s
                    (dirX , dirY) = shipDir s
                    flameLength = 20
                    baseFlamePoint = (fst center - flameLength * dirX, snd center - flameLength * dirY)
                    flameWidth = flameLength * 0.5
                    flamePoints = [baseFlamePoint]
               in flamePoints
    -- render :: Float -> Ship -> Picture
    render t s = if forward s && shouldShowFlame t then Pictures $ map drawFlamePoint (Common.animate s)
                 else Blank
        where
            drawFlamePoint (x,y) = translate x y $ color orange $ circleSolid 5.0
            shouldShowFlame time = even (floor (time / 0.1))

-- | Common functions used throughout the code base.

-- Math

normalize :: (Float, Float) -> (Float, Float)
normalize (dx, dy) =
    let magnitude = sqrt (dx * dx + dy * dy)
    in if magnitude == 0 then (0, 0) else (dx / magnitude, dy / magnitude)

magnitude :: Velocity -> Float
magnitude = sqrt . magnitudeSquared

magnitudeSquared :: Velocity -> Float
magnitudeSquared = dotProduct

addVectors :: (Float, Float) -> (Float, Float) -> (Float, Float)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVector :: (Float, Float) -> Float -> (Float, Float)
scaleVector (x, y) s = (x * s, y * s)

dotProduct :: (Float, Float) -> Float
dotProduct (x, y) = x * x + y * y

-- Other

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint angle (cx, cy) (x, y) =
    let x' = cx + (x - cx) * cos angle - (y - cy) * sin angle
        y' = cy + (x - cx) * sin angle + (y - cy) * cos angle
    in (x', y')

wrapPoint :: Point -> Point
wrapPoint (x, y) = (x', y')
    where
        x'
          | x < -(fst screenSize/2) = x + 2 * fst screenSize/2
          | x > fst screenSize/2 = x - 2 * fst screenSize/2
          | otherwise = x
        y'
          | y < -(snd screenSize/2) = y + 2 * snd screenSize/2
          | y > snd screenSize/2 = y - 2 * snd screenSize/2
          | otherwise = y

isOutOfBounds :: Point -> Bool
isOutOfBounds (x, y) = x < -(fst screenSize) || x > fst screenSize || y < -(snd screenSize) || y > snd screenSize
