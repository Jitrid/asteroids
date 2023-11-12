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
    ship      = Ship {shipLocation = (0,0), shipPos = [(-15,-15), (0,30), (15,-15), (-15,-15)], movingForward = False, shipDir = (0,1), shipSpd = (0, 0), shipRot = 0, shipHbx = (100, 100) },
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
    shipLocation = newLocation,
    shipPos      = shipPos ship  
} where
    (locX, locY) = shipLocation ship
    (dx, dy)     = shipSpd ship
    newLocation  = (locX + dx * deltaTime, locY + dy * deltaTime)


renderShip :: Ship -> Picture
renderShip ship = Translate locX locY $ Pictures $ drawPath rotatedPath
  where
    (locX, locY) = shipLocation ship
    (dx, dy)     = shipDir ship
    rotationAngle = atan2 dy dx
    rotatedPath  = map (rotatePoint rotationAngle (0,0)) (shipPos ship)
    drawPath path = map (\(x, y) -> translate x y $ color white $ circleSolid 2) path 

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint angle (cx, cy) (x, y) =
    let x' = cx + (x - cx) * cos angle - (y - cy) * sin angle
        y' = cy + (x - cx) * sin angle + (y - cy) * cos angle
    in (x', y')


createFlameShape :: Ship -> [Point]
createFlameShape s =
  let center = shipLocation s
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
