-- | The goal of this file is to define test functions, as well as some test variables to use in the tests.
-- It is not meant to be run as a standalone program, but rather to be imported by other modules.
module Tests where

import Common
import Controller

import Graphics.Gloss

-- | Ship-Asteroid collision detection

testShipAsteroidCollision :: Ship -> Asteroid -> Bool
testShipAsteroidCollision = checkShipAsteroidCollision

testShip :: Ship
testShip = Ship {
    shipCtr = (0, 0),
    shipPos = [(-15,-15), (0,30), (15,-15), (-15,-15)],
    shipDir = (0, 0),
    shipSpd = (0, 0),
    shipRot = 0,
    shipHbx = (20, 20),
    forward = False
}

testAsteroid1 :: Asteroid
testAsteroid1 = Asteroid {
    astPos = (0, 0),
    astDir = (0, 0),
    astSpd = (0, 0),
    astHbx = (20, 20),
    astSize = 20
}
testAsteroid2 :: Asteroid
testAsteroid2 = Asteroid {
    astPos = (1000, 1000),
    astDir = (0, 0),
    astSpd = (0, 0),
    astHbx = (20, 20),
    astSize = 20
}

-- | Ship-Enemy collision detection

testShipEnemyCollision :: Ship -> Enemy -> Bool
testShipEnemyCollision = checkShipEnemyCollision

testEnemy :: Enemy
testEnemy = Enemy {
    enemyPos = (0, 0),
    enemyDir = (0, 0),
    enemySpd = (0, 0),
    enemyHbx = (20, 20),
    enemyFireCD = 0
}
testEnemy2 :: Enemy
testEnemy2 = Enemy {
    enemyPos = (1000, 1000),
    enemyDir = (0, 0),
    enemySpd = (0, 0),
    enemyHbx = (20, 20),
    enemyFireCD = 0
}

-- | Out of bounds detection

testOutOfBounds :: Point -> Bool
testOutOfBounds = isOutOfBounds

