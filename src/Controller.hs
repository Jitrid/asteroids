{-# LANGUAGE UnicodeSyntax #-}

-- | This module handles time and user input.
module Controller where

import Model
import Common

import System.Random (randomRIO)
import Asteroid
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (catMaybes)

adjust :: (Int -> Int -> Int) -> GameState -> GameState
adjust f g
  | lives g' >= 3 = g' { lives = 3 }
  | lives g' <= 0 = GameOver
  | otherwise     = g'
  where g' = g { lives = f (lives g) 1 }

normalize :: (Float, Float) -> (Float, Float)
normalize (dx, dy) =
    let magnitude = sqrt (dx * dx + dy * dy)
    in if magnitude == 0 then (0, 0) else (dx / magnitude, dy / magnitude)

handleEvents :: Event -> GameState -> GameState
-- handleEvents event gs = traceShow event gs
-- starting a new game
handleEvents (EventKey (Char 'N') Down (Modifiers Down Up Up) _) _ = initialState
-- stop game from crashing when pressing keys while game over
handleEvents _ GameOver = GameOver
-- pausing the game
handleEvents (EventKey (SpecialKey s) Down _ _) gstate
    | paused gstate = pause False
    | otherwise     = pause True
        where
            pause :: Bool -> GameState
            pause b = if s == KeyEsc then gstate { paused = b } else gstate
-- mouse input
handleEvents (EventKey (MouseButton m) Down _ _) gstate
    | paused gstate = gstate -- freeze game when paused
    | otherwise = case m of
        LeftButton  -> gstate { bullets = newBullet : bullets gstate }
         where
             newBullet = Bullet {
                 bulletPos = Common.shipCenter (ship gstate),
                 bulletDir =  normalize (shipDir (ship gstate)),
                 bulletSpd = (1000, 1000) ,
                 bulletHbx = (10, 10)
             }
        RightButton -> adjust (-) gstate
        _           -> gstate
-- keyboard input
handleEvents (EventKey (Char c) Down _ _) gstate
    | paused gstate = gstate
    | otherwise = case (c, Down) of
        ('w', _) ->  gstate {
          ship = (ship gstate) { movingForward = True}
        }
        ('s',_) -> gstate {
            ship = applyBrake (ship gstate) }
        ('a',_) -> gstate {
            ship = (ship gstate) { rotDir = 1}
        } -- rotate left
        ('d',_) -> gstate {
            ship = (ship gstate) { rotDir = -1 }
        } -- rotate right

        _   -> gstate

handleEvents (EventKey (Char c) Up _ _) gstate
-- handle Key UP
    | paused gstate = gstate
    | otherwise = case (c, Up) of
        ('w', _) ->  gstate {
             ship = (ship gstate) { movingForward = False } }
        -- ('s',_) -> gstate {
        --     ship = (ship gstate) { shipSpd = (0, 0) } }
        ('a',_) -> gstate {
            ship = (ship gstate) { rotDir = 0}
        } -- rotate left
        ('d',_) -> gstate {
            ship = (ship gstate) { rotDir = 0 }
        } -- rotate right
        _   -> gstate





-- default
handleEvents _ gstate = gstate

handleInput :: Event -> GameState -> IO GameState
handleInput event gstate = return (handleEvents event gstate)

simulateGame :: Float -> GameState -> IO GameState
simulateGame deltaTime gstate@(Play { ship = s, asteroids = asts, bullets = bs, enemies = ens }) = do
    --let rotatedShip = updateRotation deltaTime s
    --let finalShip = moveShipPath deltaTime rotatedShip
    let rotatedShip = updateRotation deltaTime s
    let shipWithThrust = (if movingForward rotatedShip then applyThrust else applyBrake) rotatedShip
    let finalShip = moveShipPath deltaTime shipWithThrust
    let updatedBullets = map (moveBullet deltaTime) bs
    let updatedAsteroids = updateAsteroids deltaTime asts
    

    -- Update enemies
    enemyActions <- mapM (\enemy -> fireEnemyBullet (updateEnemy deltaTime enemy) s) ens
    let (finalUpdatedEnemies, newEnemyBullets) = unzip enemyActions
    let allBullets = updatedBullets ++ catMaybes newEnemyBullets

    --Kill outside of bounds asteroids, bullets and enemies
    let survivingAsteroids = filter (not . isOutOfBounds . astPos) updatedAsteroids
    let survivingEnemies = filter (not . isOutOfBounds . enemyPos) finalUpdatedEnemies
    let survivingBullets = filter (not . isOutOfBounds . bulletPos) allBullets

    -- Check for collisions and kill asteroids
    let notshotAsteroids = checkBulletAsteroidCollision survivingBullets survivingAsteroids
    let notcollidedAsteroids = filter (not . checkShipAsteroidCollision finalShip) notshotAsteroids

    -- Spawn new Asteroid
    shouldSpawnAst <- shouldSpawnAsteroid
    newAsteroids <- if shouldSpawnAst then fmap (:notshotAsteroids) createRandomAsteroid else return notshotAsteroids

    -- Spawn new Enemy
    shouldSpawnEn <- shouldSpawnEnemy
    newEnemies <- if shouldSpawnEn then fmap (:survivingEnemies) createRandomEnemy else return survivingEnemies

    return gstate {
        ship = finalShip,
        bullets = survivingBullets,
        asteroids = newAsteroids,
        time = time gstate + deltaTime,
        enemies = newEnemies
    }

--Hier is een begin aan collision enkel voor asteroids. is POC 
-- collisionDetected  :: Bullet -> Asteroid -> Bool
-- collisionDetected bullet asteroid = 
--     let (bx, by) = bulletPos bullet
--         (ax, ay) = astPos asteroid
--         distance = sqrt ((bx - ax)^2 + (by - ay)^2)
--     in distance < (astSize asteroid + 2)

collisionDetected :: (Point, HitboxUnit) -> (Point, HitboxUnit) -> Bool
collisionDetected ((x1, y1), (w1, h1)) ((x2,y2), (w2, h2)) =
    abs (x1 - x2) * 2 < (w1 + w2) &&
    abs (y1 - y2) * 2 < (h1 + h2)

checkBulletAsteroidCollision :: [Bullet] -> [Asteroid] -> [Asteroid]
checkBulletAsteroidCollision bullets = filter (\ast -> not (any (collisionDetected (astPos ast, astHbx ast) . (\b -> (bulletPos b, bulletHbx b))) bullets))

checkShipAsteroidCollision :: Ship -> Asteroid -> Bool
-- check if single asteroid collides with ship
checkShipAsteroidCollision ship asteroid = any (collisionDetected (Common.shipCenter ship, shipHbx ship) . (\b -> (bulletPos b, bulletHbx b))) bullets
    where bullets = map (\(x, y) -> Bullet { bulletPos = (x, y), bulletDir = (0, 0), bulletSpd = (0, 0), bulletHbx = (0, 0) }) (shipPos ship)


updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids deltaTime = map (moveAsteroid deltaTime)

updateEnemies :: Float -> [Enemy] -> [Enemy]
updateEnemies deltaTime = map (moveEnemy deltaTime)

updateEnemy :: Float -> Enemy -> Enemy
updateEnemy deltaTime enemy =
    enemy {
        enemyPos = (newX, newY), -- Update position
        enemyFireCD = newCooldown -- Update fire cooldown
    }
    where
        -- Update Position
        (currentX, currentY) = enemyPos enemy
        (velocityX, velocityY) = enemySpd enemy
        newX = currentX + velocityX * deltaTime
        newY = currentY + velocityY * deltaTime

        -- Update Cooldown
        newCooldown = max 0 (enemyFireCD enemy - deltaTime)

isOutOfBounds :: Point -> Bool
isOutOfBounds (x, y) = x < -700 || x > 700 || y < -1200 || y > 1200
--normalize and magnitude didn't work, so I made my own normalize function.
shortestPathToPlayer :: Enemy -> Ship -> Direction
shortestPathToPlayer enemy ship = normalize' (dx ,dy)
    where
        dx = fst (Common.shipCenter ship) - fst (enemyPos enemy)
        dy = snd (Common.shipCenter ship) - snd (enemyPos enemy)
        normalize' (x, y) = let mag = sqrt (x*x + y*y) in (x / mag, y / mag)

fireEnemyBullet :: Enemy -> Ship -> IO (Enemy, Maybe Bullet)
fireEnemyBullet enemy playerShip
    | enemyFireCD enemy <= 0 = do

        let bulletDir = shortestPathToPlayer enemy playerShip
        let newEnemy = enemy { enemyFireCD = 2.0 }
        return (newEnemy, Just Bullet {
            bulletPos = enemyPos enemy,
            bulletDir = bulletDir,
            bulletSpd = (1000, 1000),
            bulletHbx = (10, 10)
        })
    | otherwise = return (enemy, Nothing)

--Ai ai ai , dit kunnen we samenvoegen met de shouldSpawnEnemy functie, nu is het een beetje dubbelop, maar ik heb geen zin om het te veranderen. 
shouldSpawnAsteroid :: IO Bool
shouldSpawnAsteroid = do
    let probability :: Double
        probability = 0.05  -- 5%

    randomValue <- randomRIO (0.0, 1.0 :: Double)
    return (randomValue < probability)

shouldSpawnEnemy :: IO Bool
shouldSpawnEnemy = do
    let probability :: Double
        probability = 0.01  -- .1%

    randomValue <- randomRIO (0.0, 1.0 :: Double)
    return (randomValue < probability)




moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid deltaTime asteroid = asteroid {
    astPos = (newX, newY)
}
  where
    -- Current position of the asteroid
    (currentX, currentY) = astPos asteroid

    -- Current velocity (speed and direction) of the asteroid
    (velocityX, velocityY) = astSpd asteroid

    -- Calculate new position
    newX = currentX + velocityX * deltaTime
    newY = currentY + velocityY * deltaTime


moveEnemy :: Float -> Enemy -> Enemy
moveEnemy deltaTime enemy = enemy {
    enemyPos = (newX, newY)
}
  where
    -- Current position of the asteroid
    (currentX, currentY) = enemyPos enemy

    -- Current velocity (speed and direction) of the asteroid
    (velocityX, velocityY) = enemySpd enemy

    -- Calculate new position
    newX = currentX + velocityX * deltaTime
    newY = currentY + velocityY * deltaTime

moveBullet :: Float -> Bullet -> Bullet
moveBullet deltaTime bullet = bullet {
    bulletPos = (x + vx * speed * deltaTime, y + vy * speed * deltaTime)
} where
    (x, y) = bulletPos bullet
    speed =  fst (bulletSpd bullet)
    (vx, vy) = bulletDir bullet

--     let updatedShip = updateRotation deltaTime $ updateShip deltaTime s
--     in return gstate {
--         ship = updateShip deltaTime s,
--         time = time gstate + deltaTime
--     }
--     where 
--         updateShip = Model.moveShipPath
--         updateRotation dt ship = ship { shipRot = 150 } 
-- simulateGame _ gstate = return gstate 

 -- No update if game is over
-- ADD PAUSING HERE

-- updateRotation :: Float -> Ship -> Ship
-- updateRotation dt ship = ship { shipDir = newDirection }
--   where
--     rotationSpeed = 1 -- Adjust this value for slower rotation
--     angleChange = rotDir ship * rotationSpeed * dt
--     (dx, dy) = shipDir ship
--     currentAngle = atan2 dy dx
--     newAngle = currentAngle + angleChange
--     newDirection = (cos newAngle, sin newAngle)

-- updateRotation' :: Float -> Ship -> Ship
-- updateRotation' dt ship = ship { shipRot = newRot }
--   where
--     rotationSpeed = 1 -- Rotation speed factor
--     newRot = shipRot ship + rotDir ship * rotationSpeed * dt

updateRotation :: Float -> Ship -> Ship
updateRotation dt ship = 
    ship { shipDir = if rotDir ship /= 0 
                     then rotateVector (angleChange * rotDir ship) (shipDir ship) 
                     else shipDir ship }
    where 
        rotationSpeed = 1.05
        angleChange = rotationSpeed * dt



rotateShipPath :: Ship -> Ship
rotateShipPath ship = ship { shipPos = rotatedPath }
  where
    (dx, dy) = shipDir ship
    rotationAngle = atan2 dy dx * 180 / pi
    center = Model.shipCenter (shipPos ship)
    rotatedPath = map (Controller.rotatePoint rotationAngle center) (shipPos ship)

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint angle (cx, cy) (x, y) =
    let x' = cos angle * (x - cx) - sin angle * (y - cy) + cx
        y' = sin angle * (x - cx) + cos angle * (y - cy) + cy
    in (x', y')



rotateShip :: Float -> Ship -> Ship
rotateShip r ship = ship { shipPos = map (rotatePoint r center) (shipPos ship) }
  where
    center = Common.shipCenter ship
    rotatePoint :: Float -> Point -> Point -> Point
    rotatePoint r' (cx, cy) (x, y) =
        let x' = cos r' * (x - cx) - sin r' * (y - cy) + cx
            y' = sin r' * (x - cx) + cos r' * (y - cy) + cy
        in (x', y')


applyThrust :: Ship -> Ship
applyThrust ship = ship { shipSpd = clampedNewVelocity }
  where
    thrustAmount = 10
    (dx, dy) = shipDir ship
    angle = atan2 dy dx
    newVelocity = (shipSpd ship) `addVectors` (cos angle * thrustAmount, sin angle * thrustAmount)
    clampedNewVelocity = if magnitude newVelocity > 600 then
                           newVelocity `scaleVector` (600 / magnitude newVelocity)
                         else
                           newVelocity

-- applyBrake :: Ship -> Ship
-- applyBrake ship = ship { shipSpd = newVelocity }
--   where
--     brakeAmount = 20
--     (dx, dy) = shipSpd ship
--     velocityMagnitude = sqrt (dx * dx + dy * dy)

--     newVelocity = if velocityMagnitude > brakeAmount then
--                     (dx - (dx / velocityMagnitude * brakeAmount),
--                      dy - (dy / velocityMagnitude * brakeAmount))
--                   else
--                     (0, 0) -- Willen we naar achter bewegen als we al stilstaan? Dankjewel copilot voor deze lijpe comment generatie :D
applyBrake :: Ship -> Ship
applyBrake ship = ship { shipSpd = newVelocity }
  where 
    brakeAmount = 0.95
    --newVelocity = (shipSpd ship) `scaleVector` (1 - breakAmount / magnitude (shipSpd ship))
    newVelocity = if magnitude (shipSpd ship) > 0 then scaleVector (shipSpd ship) brakeAmount else (0, 0)


magnitude :: Velocity -> Float
magnitude = sqrt . magnitudeSquared

magnitudeSquared :: Velocity -> Float
magnitudeSquared = dotProduct

--Vector Math
addVectors :: (Float, Float) -> (Float, Float) -> (Float, Float)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVector :: (Float, Float) -> Float -> (Float, Float)
scaleVector (x, y) s = (x * s, y * s)

dotProduct :: (Float, Float) -> Float
dotProduct (x, y) = x * x + y * y

rotateVector :: Float -> Direction -> Direction
rotateVector r (x, y) = (cos r * x - sin r * y, sin r * x + cos r * y)