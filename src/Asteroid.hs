module Asteroid where
import System.Random (randomRIO)
import Common

-- Function to create a random asteroid
createRandomAsteroid :: IO Asteroid
createRandomAsteroid = do
    -- Random position (adjust ranges according to your game's world size)
    x <- randomRIO (-700, 700)
    y <- randomRIO (-1200, 1200)

    -- Random direction
    dirX <- randomRIO (-1, 1)
    dirY <- randomRIO (-1, 1)

    -- Random speed
    speed <- randomRIO (50, 500)

    -- Define size and hitbox (adjust as needed)
    size <- randomRIO (20,80) -- Example size
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
    x <- randomRIO (-700, 700)
    y <- randomRIO (-1200, 1200)

    dirX <- randomRIO (-1, 1)
    dirY <- randomRIO (-1, 1)

    speed <- randomRIO (100, 300)

    let size = 20

    return Enemy {
        enemyPos = (x, y),
        enemyDir = (dirX, dirY),
        enemySpd = (dirX * speed, dirY * speed),
        enemyHbx = (size, size),
        enemyDif = Easy,
        enemyFireCD = 2
    }