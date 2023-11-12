{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import Common
import Graphics.Gloss
import Graphics.UI.GLUT (screenSize)

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
    ship      = Ship { shipPos = [(-25,-25), (0,50), (25,-25), (-25,-25)], movingForward = False, shipDir = (180,1), shipSpd = (0, 0), shipRot = 45, rotDir = 0, shipHbx = (100, 100) },
    asteroids = [],
    enemies   = [],
    bullets   = [],
    score     = 0,
    lives     = 3,
    level     = 1,
    time      = 0,
    paused    = False
}



-- Update Point based on direction and speed
moveShipPath :: Float -> Ship -> Ship
moveShipPath deltaTime ship = ship {
    shipPos = wrappedShipPos

}  where
        updatedShipPos = map (\(x, y) -> (x + dx * deltaTime, y + dy * deltaTime)) (shipPos ship)
        updatedCenter = Model.shipCenter updatedShipPos
        wrappedCenter = wrapPos updatedCenter
        (updatedCenterX, updatedCenterY) = updatedCenter
        (wrappedCenterX, wrappedCenterY) = wrappedCenter
        wrappedShipPos = map (\(x, y) -> (x + wrappedCenterX - updatedCenterX, y + wrappedCenterY - updatedCenterY)) updatedShipPos
        screenW = 700
        screenH = 1200
        (dx, dy) = shipSpd ship
        updatePoint (x, y) = (x + dx * deltaTime, y + dy * deltaTime)
        wrapPos (x, y) = (wrapCoord x screenH, wrapCoord y screenW)
        wrapCoord x max
            | x < - (max/2) = x + max
            | x > (max /2)   = x - max
            | otherwise = x


shipCenter :: [Point] -> Point
shipCenter points =
    let (sumX, sumY, count) = foldr (\(x, y) (accX, accY, n) -> (accX + x, accY + y, n + 1)) (0, 0, 0) points
    in (sumX / count, sumY / count)


renderShip :: Ship -> Picture
renderShip ship = Pictures $ map drawPath rotatedPath
  where
    center = Model.shipCenter (shipPos ship)
    (dx, dy) = shipDir ship
    rotationAngle = atan2 dy dx

    rotatedPath = map (rotatePoint rotationAngle center) (shipPos ship)

    drawPath (x, y) = translate x y $ color shipColor $ circleSolid pointSize

    shipColor = white  -- Define the color of the ship
    pointSize = 5.0    -- Define the size of each point in the ship's path

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint angle (cx, cy) (x, y) =
    let x' = cx + (x - cx) * cos angle - (y - cy) * sin angle
        y' = cy + (x - cx) * sin angle + (y - cy) * cos angle
    in (x', y')

renderBullet :: Bullet -> Picture
renderBullet bullet = uncurry translate (bulletPos bullet) $ color bulletColor $ circleSolid bulletSize
  where
    bulletColor = red  -- Define the color of the bullet
    bulletSize = 1.0     -- Define the size of the bullet

renderAsteroid :: Asteroid -> Picture
renderAsteroid asteroid = uncurry translate (astPos asteroid) $ color asteroidColor $ circleSolid (astSize asteroid)
  where
    asteroidColor = green  -- Define the color of the asteroid

renderEnemy :: Enemy -> Picture
renderEnemy enemy = uncurry translate (enemyPos enemy) $ color enemyColor $ circleSolid 5
  where
    enemyColor = blue  -- Define the color of the enemy 
