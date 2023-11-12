{-# LANGUAGE UnicodeSyntax #-}

module Controller where

import Model
import Common
import Random

import System.Random (randomRIO)
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
                 bulletPos = shipCtr (ship gstate) `addVectors` (shipDir (ship gstate) `scaleVector` 30),
                 bulletDir = normalize (shipDir (ship gstate)),
                 bulletSpd = (300, 300) ,
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
        'a' -> gstate {
            ship = (ship gstate) { shipRot = 1}
        }
        'd' -> gstate {
            ship = (ship gstate) { shipRot = -1 }
        }
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
    | otherwise = do
        let s = ship gstate
        let asts = asteroids gstate
        let bs = bullets gstate
        let ens = enemies gstate

        let rotatedShip = Common.rotate deltaTime s
        let shipWithThrust = (if forward rotatedShip then applyThrust else applyBrake) rotatedShip
        let finalShip = move deltaTime shipWithThrust
        let updatedBullets = map (move deltaTime) bs
        let updatedAsteroids = map (move deltaTime) asts

        -- Update enemies
        let enemyActions = map (\enemy -> fireEnemyBullet (move deltaTime enemy) s) ens
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
        let (shotAsteroids, notshotAsteroids, survivingBulletsAfterAsteroids) = checkBulletAsteroidCollision survivingBullets survivingAsteroids
        let newAsteroidsfromsplit = concatMap splitAsteroid shotAsteroids
        let allAsteroids = notshotAsteroids ++ newAsteroidsfromsplit
        let score = currentScore + (length shotAsteroids * 100)

        -- Check for collisions and kill enemies
        let (shotEnemies, notshotEnemies) = checkBulletEnemyCollision survivingBulletsAfterAsteroids survivingEnemies
        let score' = score + (length shotEnemies * 1250)

        -- Spawn new Asteroid
        shouldSpawnAst <- shouldSpawnAsteroid
        newAsteroids <- if shouldSpawnAst then fmap (:allAsteroids) createRandomAsteroid else return allAsteroids

        -- Spawn new Enemy
        shouldSpawnEn <- shouldSpawnEnemy
        newEnemies <- if shouldSpawnEn then fmap (:notshotEnemies) createRandomEnemy else return notshotEnemies
        if finalHP /= currentHP && finalHP > 0
            then return initialState { lives = finalHP, score = score', asteroids = [] }
            else if finalHP == 0
                then do 
                    appendFile "scores.txt" (show score' ++ "\n")
                    return GameOver
                else return gstate {
                    score = score',
                    lives = finalHP,
                    ship = finalShip,
                    bullets = survivingBulletsAfterAsteroids,
                    asteroids = newAsteroids,
                    time = time gstate + deltaTime,
                    enemies = newEnemies
                }

-- | Ship Movement

applyThrust :: Ship -> Ship
applyThrust ship = ship { shipSpd = clampedNewVelocity }
    where
        thrustAmount       = 10
        (dx, dy)           = shipDir ship
        angle              = atan2 dy dx
        newVelocity        = shipSpd ship `addVectors` (cos angle * thrustAmount, sin angle * thrustAmount)
        clampedNewVelocity = if magnitude newVelocity > 250 then newVelocity `scaleVector` (250 / magnitude newVelocity)
                                else newVelocity

applyBrake :: Ship -> Ship
applyBrake ship = ship { shipSpd = newVelocity }
    where
        brakeAmount = 0.95
        newVelocity = if magnitude (shipSpd ship) > 0 then scaleVector (shipSpd ship) brakeAmount
                        else (0, 0)

-- | Collision Detection

collisionDetected :: (Point, HitboxUnit) -> (Point, HitboxUnit) -> Bool
collisionDetected ((x1, y1), (w1, h1)) ((x2,y2), (w2, h2)) = abs (x1 - x2) * 2 < (w1 + w2) && abs (y1 - y2) * 2 < (h1 + h2)

checkShipAsteroidCollision :: Ship -> Asteroid -> Bool
checkShipAsteroidCollision ship asteroid =
    collisionDetected (shipCtr ship, shipHbx ship) (astPos asteroid, astHbx asteroid)

checkShipEnemyCollision :: Ship -> Enemy -> Bool
checkShipEnemyCollision ship enemy =
    collisionDetected (shipCtr ship, shipHbx ship) (enemyPos enemy, enemyHbx enemy)

checkBulletAsteroidCollision :: [Bullet] -> [Asteroid] -> ([Asteroid], [Asteroid], [Bullet])
checkBulletAsteroidCollision bullets asteroids =
    let (shot, notShot, bulletsThatHit) = foldr checkAndSplit ([], [], []) asteroids
    in (shot, notShot, [b | b <- bullets, not (b `isHitByAny` bulletsThatHit)])
    where
        checkAndSplit ast (shotAsts, notShotAsts, hitBullets) =
            let (hit, notHit) = foldr (checkBulletHit ast) ([], []) bullets
            in (if not (null hit) then ast : shotAsts else shotAsts,
                if null hit then ast : notShotAsts else notShotAsts,
                hit ++ hitBullets)

        checkBulletHit ast bullet (hit, notHit) =
            if collisionDetected (bulletPos bullet, bulletHbx bullet) (astPos ast, astHbx ast)
            then (bullet : hit, notHit)
            else (hit, bullet : notHit)

        isHitByAny bullet = any (\hitBullet -> bulletPos bullet == bulletPos hitBullet)

checkBulletEnemyCollision :: [Bullet] -> [Enemy] -> ([Enemy], [Enemy])
checkBulletEnemyCollision bullets = foldr checkAndSplit ([], [])
    where
        checkAndSplit enemy (shot, notShot)
            | any (flip collisionDetected (enemyPos enemy, enemyHbx enemy) . (\b -> (bulletPos b, bulletHbx b))) bullets = (enemy : shot, notShot)
            | otherwise = (shot, enemy : notShot)

-- | Enemy Logic

shortestPathToPlayer :: Enemy -> Ship -> Direction
shortestPathToPlayer enemy ship = normalize (dx ,dy)
    where
        dx = fst (shipCtr ship) - fst (enemyPos enemy)
        dy = snd (shipCtr ship) - snd (enemyPos enemy)

fireEnemyBullet :: Enemy -> Ship -> (Enemy, Maybe Bullet)
fireEnemyBullet enemy playerShip
    | enemyFireCD enemy <= 0 =
        let bulletDir = shortestPathToPlayer enemy playerShip
            newEnemy = enemy { enemyFireCD = 2.0 }

        in (newEnemy, Just Bullet {
            bulletPos = enemyPos enemy `addVectors` (bulletDir `scaleVector` 25),
            bulletDir = bulletDir,
            bulletSpd = (300, 300),
            bulletHbx = (10, 10)
        })
    | otherwise = (enemy, Nothing)
