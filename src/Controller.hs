{-# LANGUAGE UnicodeSyntax #-}

-- | This module handles time and user input.
module Controller where

import Model
import Common

import System.Random (randomRIO)
import Asteroid
import JSONSAVER
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
                 bulletPos = shipCtr (ship gstate),  
                 bulletDir = normalize (shipDir (ship gstate)),
                 bulletSpd = (1000, 1000) ,
                 bulletHbx = (10, 10)
             }
        _           -> gstate
-- keyboard input
handleEvents (EventKey (Char c) Down _ _) gstate
    | paused gstate = gstate
    | otherwise = case c of
        'w' ->  gstate {
          ship = (ship gstate) { forward = True}
        }
        's' -> gstate {
            ship = applyBrake (ship gstate) }
        'a' -> gstate {
            ship = (ship gstate) { shipRot = 1}
        } -- rotate left
        'd' -> gstate {
            ship = (ship gstate) { shipRot = -1 }
        } -- save game
        'p' -> gstate { 
            paused = True
            saveGameState "save.json" gstate            
             }--load
        'l' -> loadGameState "save.json"
        _   -> gstate
handleEvents (EventKey (Char c) Up _ _) gstate
    | paused gstate = gstate
    | otherwise = case c of
        'w' ->  gstate {
             ship = (ship gstate) { forward = False } }
        'a' -> gstate {
            ship = (ship gstate) { shipRot = 0} -- reset rotation
        }
        'd' -> gstate {
            ship = (ship gstate) { shipRot = 0 }
        }
        _   -> gstate
-- default
handleEvents _ gstate = gstate

handleInput :: Event -> GameState -> IO GameState
handleInput event gstate = return (handleEvents event gstate)

-- | Game Simulation

simulateGame :: Float -> GameState -> IO GameState
simulateGame _ GameOver = return GameOver
simulateGame deltaTime gstate
    | paused gstate = return gstate
    -- | 
    | otherwise = do
        let s = ship gstate
        let asts = asteroids gstate
        let bs = bullets gstate
        let ens = enemies gstate

        let rotatedShip = updateRotation deltaTime s
        let shipWithThrust = (if forward rotatedShip then applyThrust else applyBrake) rotatedShip
        let finalShip = moveShipPath deltaTime shipWithThrust
        let updatedBullets = map (move deltaTime) bs
        let updatedAsteroids = map (move deltaTime) asts

        -- Update enemies
        enemyActions <- mapM (\enemy -> fireEnemyBullet (move deltaTime enemy) s) ens
        let (finalUpdatedEnemies, newEnemyBullets) = unzip enemyActions
        let allBullets = updatedBullets ++ catMaybes newEnemyBullets

        let currentHP = lives gstate
        let currentScore = score gstate

        -- check if ship collided with asteroid
        let collided = if any (checkShipAsteroidCollision finalShip) updatedAsteroids then currentHP - 1 else currentHP

        -- check if ship has collided with enemy 
        let collided' = if any (checkShipEnemyCollision finalShip) finalUpdatedEnemies then collided - 1 else collided

        -- check if ship collided with enemy bullet
        let collided'' = if any (collisionDetected (shipCtr s, shipHbx finalShip) . (\b -> (bulletPos b, bulletHbx b))) allBullets then collided' - 1 else collided'

        -- check if HP is still the same, otherwise game over if HP is 0
        let finalHP = if collided'' /= currentHP then collided'' else currentHP

        --Kill outside of bounds asteroids, bullets and enemies
        let survivingAsteroids = filter (not . isOutOfBounds . astPos) updatedAsteroids
        let survivingEnemies = filter (not . isOutOfBounds . enemyPos) finalUpdatedEnemies
        let survivingBullets = filter (not . isOutOfBounds . bulletPos) allBullets


        -- Check for collisions and kill asteroids
        let (shotAsteroids, notshotAsteroids) = checkBulletAsteroidCollision survivingBullets survivingAsteroids
        let newAsteroidsfromsplit = concatMap splitAsteroid shotAsteroids
        let allAsteroids = notshotAsteroids ++ newAsteroidsfromsplit
        let score = currentScore + (length shotAsteroids * 100)

        -- Check for collisions and kill enemies
        let (shotEnemies, notshotEnemies) = checkBulletEnemyCollision survivingBullets survivingEnemies
        let score' = score + (length shotEnemies * 250)

        -- Spawn new Asteroid
        shouldSpawnAst <- shouldSpawnAsteroid
        newAsteroids <- if shouldSpawnAst then fmap (:allAsteroids) createRandomAsteroid else return allAsteroids

        -- Spawn new Enemy
        shouldSpawnEn <- shouldSpawnEnemy
        newEnemies <- if shouldSpawnEn then fmap (:notshotEnemies) createRandomEnemy else return notshotEnemies
        if finalHP /= currentHP && finalHP > 0
            then return initialState { lives = finalHP, score = score, asteroids = [] }
            else if finalHP == 0
                then return GameOver
                else return gstate {
                    score = score,
                    lives = finalHP,
                    ship = finalShip,
                    bullets = survivingBullets,
                    asteroids = newAsteroids,
                    time = time gstate + deltaTime,
                    enemies = newEnemies
                }

checkBulletAsteroidCollision :: [Bullet] -> [Asteroid] -> ([Asteroid], [Asteroid])
checkBulletAsteroidCollision bullets = foldr checkAndSplit ([], [])
    where
        checkAndSplit ast (shot, notShot)
            | any (flip collisionDetected (astPos ast, astHbx ast) . (\b -> (bulletPos b, bulletHbx b))) bullets = (ast : shot, notShot)
            | otherwise = (shot, ast : notShot)

checkBulletEnemyCollision :: [Bullet] -> [Enemy] -> ([Enemy], [Enemy])
checkBulletEnemyCollision bullets = foldr checkAndSplit ([], [])
    where
        checkAndSplit enemy (shot, notShot)
            | any (flip collisionDetected (enemyPos enemy, enemyHbx enemy) . (\b -> (bulletPos b, bulletHbx b))) bullets = (enemy : shot, notShot)
            | otherwise = (shot, enemy : notShot)

checkShipAsteroidCollision :: Ship -> Asteroid -> Bool
checkShipAsteroidCollision ship asteroid = 
    collisionDetected (shipCtr ship, shipHbx ship) (astPos asteroid, astHbx asteroid)


checkShipEnemyCollision :: Ship -> Enemy -> Bool
checkShipEnemyCollision ship enemy = 
    collisionDetected (shipCtr ship, shipHbx ship) (enemyPos enemy, enemyHbx enemy)

splitAsteroids :: [Asteroid] -> Ship -> ([Asteroid], [Asteroid])
splitAsteroids asteroids ship = foldr split ([], []) asteroids
  where
    split ast (collided, notCollided)
        | checkShipAsteroidCollision ship ast = (ast : collided, notCollided)
        | otherwise = (collided, ast : notCollided)

shortestPathToPlayer :: Enemy -> Ship -> Direction
shortestPathToPlayer enemy ship = normalize' (dx ,dy)
    where
        dx = fst (shipCtr ship) - fst (enemyPos enemy)
        dy = snd (shipCtr ship) - snd (enemyPos enemy)
        normalize' (x, y) = let mag = sqrt (x*x + y*y) in (x / mag, y / mag)

fireEnemyBullet :: Enemy -> Ship -> IO (Enemy, Maybe Bullet)
fireEnemyBullet enemy playerShip
    | enemyFireCD enemy <= 0 = do

        let bulletDir = shortestPathToPlayer enemy playerShip
        let newEnemy = enemy { enemyFireCD = 2.0 }
        return (newEnemy, Just Bullet {
            bulletPos = enemyPos enemy,
            bulletDir = bulletDir,
            bulletSpd = (300, 300),
            bulletHbx = (10, 10)
        })
    | otherwise = return (enemy, Nothing)

updateRotation :: Float -> Ship -> Ship
updateRotation dt ship = 
    ship { shipDir = if shipRot ship /= 0 
                     then (cosR * dx - sinR * dy, sinR * dx + cosR * dy) 
                     else shipDir ship }
    where 
        rotationAmount = shipRot ship * dt 
        cosR = cos rotationAmount
        sinR = sin rotationAmount
        (dx, dy) = shipDir ship
        rotationSpeed = 1.05
        angleChange = rotationSpeed * dt

rotateShipPath :: Ship -> Ship
rotateShipPath ship = ship { shipPos = rotatedPath }
  where
    (dx, dy) = shipDir ship
    rotationAngle = atan2 dy dx * 180 / pi
    center = shipCtr ship
    rotatedPath = map (rotatePoint rotationAngle center) (shipPos ship)


rotateShip :: Float -> Ship -> Ship
rotateShip r ship = ship { shipPos = map (rotatePoint r (shipCtr ship)) (shipPos ship) }

applyThrust :: Ship -> Ship
applyThrust ship = ship { shipSpd = clampedNewVelocity }
    where
        thrustAmount       = 10
        (dx, dy)           = shipDir ship
        angle              = atan2 dy dx
        newVelocity        = shipSpd ship `addVectors` (cos angle * thrustAmount, sin angle * thrustAmount)
        clampedNewVelocity = if magnitude newVelocity > 400 then newVelocity `scaleVector` (400 / magnitude newVelocity)
                                else newVelocity

applyBrake :: Ship -> Ship
applyBrake ship = ship { shipSpd = newVelocity }
    where 
        brakeAmount = 0.95
        newVelocity = if magnitude (shipSpd ship) > 0 then scaleVector (shipSpd ship) brakeAmount 
                        else (0, 0)
