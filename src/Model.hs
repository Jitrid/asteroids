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
    ship      = Ship { shipPos = [(-15,-15), (0,30), (15,-15), (-15,-15)], movingForward = False, shipDir = (0,1), shipSpd = (0, 0), shipRot = 0, shipHbx = (100, 100) },
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
        updatedCenter = shipCenter updatedShipPos
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



renderShip :: Ship -> Picture
renderShip ship = Pictures $ drawPath rotatedPath
  where
    center = shipCenter (shipPos ship)
    (dx, dy) = shipDir ship
    rotationAngle = atan2 dx dy

    rotatedPath = map (rotatePoint rotationAngle center) (shipPos ship)
    drawPath path = map (\(x, y) -> translate x y $ color white $ line path) path

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint angle (cx, cy) (x, y) =
    let x' = cx + (x - cx) * cos angle - (y - cy) * sin angle
        y' = cy + (x - cx) * sin angle + (y - cy) * cos angle
    in (x', y')


createFlameShape :: Ship -> [Point]
createFlameShape s =
  let center = shipCenter (shipPos s)
      flamePoints = [(-5, -10), (0, -20), (5, -10)]
  in map (translatePoint center) flamePoints


--translatePoint is a function that will translate a point based on the ship's center
translatePoint :: Point -> Point -> Point
translatePoint (cx, cy) (x, y) = (x + cx, y + cy)

splitAsteroid :: Asteroid -> [Asteroid]
splitAsteroid ast
  | astSize ast > 40 = [ast { astSize = newSize, astPos = (x + offset, y) }, 
                        ast { astSize = newSize, astPos = (x - offset, y) }]
  | otherwise = []
  where
    newSize = astSize ast / 2
    (x, y) = astPos ast
    offset = newSize / 2 -- adjust as needed


renderFlame :: Ship -> Float -> Picture
renderFlame s t =
  if movingForward s && shouldShowFlame t 
  then Pictures $ map drawFlamePoint (createFlameShape s)
  else Blank

drawFlamePoint :: Point -> Picture
drawFlamePoint (x, y) = translate x y $ color flameColor $ circleSolid pointSize
  where
    flameColor = orange
    pointSize = 5.0

shouldShowFlame :: Float -> Bool
shouldShowFlame time =
  let rate = 0.1
  in even (floor (time / rate))



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
