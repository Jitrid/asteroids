module Random where

import Common

import System.Random

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

shouldSpawn :: Double -> IO Bool
shouldSpawn prob = do
    randomValue <- randomRIO (0.0, 1.0 :: Double)
    return (randomValue < prob)
