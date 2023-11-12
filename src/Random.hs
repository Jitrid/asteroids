module Random where

import System.Random (randomRIO)
import Common

-- Function to create a random asteroid
createRandomAsteroid :: IO Asteroid
createRandomAsteroid = do
    x <- randomRIO (-400, 400)
    y <- randomRIO (-750, 750)

    dirX <- randomRIO (-1, 1)
    dirY <- randomRIO (-1, 1)

    speed <- randomRIO (50, 500)

    size <- randomRIO (20,80)
    let hitbox = (size, size)

    return Asteroid {
        astPos  = (x, y),
        astDir  = (dirX, dirY),
        astSpd  = (dirX * speed, dirY * speed),
        astHbx  = hitbox,
        astSize = size
    }

createRandomEnemy :: IO Enemy
createRandomEnemy = do
    x <- randomRIO (-400, 400)
    y <- randomRIO (-750, 750)

    dirX <- randomRIO (-1, 1)
    dirY <- randomRIO (-1, 1)

    speed <- randomRIO (100, 300)

    let size = 20

    return Enemy {
        enemyPos = (x, y),
        enemyDir = (dirX, dirY),
        enemySpd = (dirX * speed, dirY * speed),
        enemyHbx = (size, size),
        enemyFireCD = 2
    }

shouldSpawnAsteroid :: IO Bool
shouldSpawnAsteroid = do
    let probability :: Double
        probability = 0.1  -- 5%

    randomValue <- randomRIO (0.0, 1.0 :: Double)
    return (randomValue < probability)

shouldSpawnEnemy :: IO Bool
shouldSpawnEnemy = do
    let probability :: Double
        probability = 0.02  -- .1%

    randomValue <- randomRIO (0.0, 1.0 :: Double)
    return (randomValue < probability)
