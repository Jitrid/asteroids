{-# LANGUAGE UnicodeSyntax #-}

-- | This module handles time and user input.
module Controller where

import Model
import Common

import System.Random (randomRIO)
import Asteroid
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (catMaybes)

-- | Handle user input

handleEvents :: Event -> GameState -> GameState
-- starting a new game
handleEvents (EventKey (Char 'N') Down _ _) _ = initialState
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
                 bulletPos = head (shipPos (ship gstate) ++ [normalize (shipDir (ship gstate))]),  
                 bulletDir =  normalize (shipDir (ship gstate)),
                 bulletSpd = (1000, 1000) ,
                 bulletHbx = (10, 10)
             }
        _           -> gstate
-- keyboard input
handleEvents (EventKey (Char c) Down _ _) gstate
    | paused gstate = gstate
    | otherwise = case c of
        'w' ->  gstate {
          ship = (ship gstate) { movingForward = True}
        }
        's' -> gstate {
            ship = applyBrake (ship gstate) }
        'a' -> gstate {
            ship = (ship gstate) { shipRot = -1}
        } -- rotate left
        'd' -> gstate {
            ship = (ship gstate) { shipRot = 1 }
        } -- rotate right
        _   -> gstate
handleEvents (EventKey (Char c) Up _ _) gstate
    | paused gstate = gstate
    | otherwise = case c of
        'w' ->  gstate {
             ship = (ship gstate) { movingForward = False } }
        'a' -> gstate {
            ship = (ship gstate) { shipRot = 0}
        } -- rotate left
        'd' -> gstate {
            ship = (ship gstate) { shipRot = 0 }
        } -- rotate right
        _   -> gstate
-- default
handleEvents _ gstate = gstate

handleInput :: Event -> GameState -> IO GameState
handleInput event gstate = return (handleEvents event gstate)

-- | Other

simulateGame :: Float -> GameState -> IO GameState
simulateGame deltaTime gstate@(Play { ship = s, asteroids = asts, bullets = bs, enemies = ens }) = do
    let rotatedShip = updateRotation deltaTime s
    let shipWithThrust = (if movingForward rotatedShip then applyThrust else applyBrake) rotatedShip
    let finalShip = moveShipPath deltaTime shipWithThrust
    let updatedBullets = map (move deltaTime) bs
    let updatedAsteroids = updateAsteroids deltaTime asts
    
    -- Update enemies
    enemyActions <- mapM (\enemy -> fireEnemyBullet (move deltaTime enemy) s) ens
    let (finalUpdatedEnemies, newEnemyBullets) = unzip enemyActions
    let allBullets = updatedBullets ++ catMaybes newEnemyBullets

    -- check for ship -asteroid colision: 
    --Kill outside of bounds asteroids, bullets and enemies
    let survivingAsteroids = filter (not . isOutOfBounds . astPos) updatedAsteroids
    let survivingEnemies = filter (not . isOutOfBounds . enemyPos) finalUpdatedEnemies
    let survivingBullets = filter (not . isOutOfBounds . bulletPos) allBullets

    -- Check for collisions and kill asteroids
    let (shotAsteroids, notshotAsteroids) = checkBulletAsteroidCollision survivingBullets survivingAsteroids
    let newAsteroidsfromsplit = concatMap splitAsteroid shotAsteroids
    let allAsteroids = notshotAsteroids ++ newAsteroidsfromsplit

    -- Spawn new Asteroid
    shouldSpawnAst <- shouldSpawnAsteroid
    newAsteroids <- if shouldSpawnAst then fmap (:allAsteroids) createRandomAsteroid else return allAsteroids

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

collisionDetected :: (Point, HitboxUnit) -> (Point, HitboxUnit) -> Bool
collisionDetected ((x1, y1), (w1, h1)) ((x2,y2), (w2, h2)) =
    abs (x1 - x2) * 2 < (w1 + w2) &&
    abs (y1 - y2) * 2 < (h1 + h2)

checkBulletAsteroidCollision :: [Bullet] -> [Asteroid] -> ([Asteroid], [Asteroid])
checkBulletAsteroidCollision bullets asteroids = foldr checkAndSplit ([], []) asteroids
  where
    checkAndSplit ast (shot, notShot)
      | any (flip collisionDetected (astPos ast, astHbx ast) . (\b -> (bulletPos b, bulletHbx b))) bullets = (ast : shot, notShot)
      | otherwise = (shot, ast : notShot)


checkShipAsteroidCollision :: Ship -> Asteroid -> Bool
checkShipAsteroidCollision ship asteroid = 
    collisionDetected (shipCenter (shipPos ship), shipHbx ship) (astPos asteroid, astHbx asteroid)

splitAsteroids :: [Asteroid] -> Ship -> ([Asteroid], [Asteroid])
splitAsteroids asteroids ship = foldr split ([], []) asteroids
  where
    split ast (collided, notCollided)
        | checkShipAsteroidCollision ship ast = (ast : collided, notCollided)
        | otherwise = (collided, ast : notCollided)

updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids deltaTime = map (move deltaTime)

shortestPathToPlayer :: Enemy -> Ship -> Direction
shortestPathToPlayer enemy ship = normalize' (dx ,dy)
    where
        dx = fst (shipCenter (shipPos ship)) - fst (enemyPos enemy)
        dy = snd (shipCenter (shipPos ship)) - snd (enemyPos enemy)
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

updateRotation :: Float -> Ship -> Ship
updateRotation dt ship = 
    ship { shipDir = if shipRot ship /= 0 
                     then rotateVector (angleChange * shipRot ship) (shipDir ship) 
                     else shipDir ship }
    where 
        rotationSpeed = 1.05
        angleChange = rotationSpeed * dt

rotateShipPath :: Ship -> Ship
rotateShipPath ship = ship { shipPos = rotatedPath }
  where
    (dx, dy) = shipDir ship
    rotationAngle = atan2 dy dx * 180 / pi
    center = shipCenter (shipPos ship)
    rotatedPath = map (rotatePoint rotationAngle center) (shipPos ship)


rotateShip :: Float -> Ship -> Ship
rotateShip r ship = ship { shipPos = map (rotatePoint r center) (shipPos ship) }
  where
    center = shipCenter (shipPos ship)


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

applyBrake :: Ship -> Ship
applyBrake ship = ship { shipSpd = newVelocity }
  where 
    brakeAmount = 0.95
    newVelocity = if magnitude (shipSpd ship) > 0 then scaleVector (shipSpd ship) brakeAmount else (0, 0)
