-- and ship asteroid collision:

-- | Test if ship collides with asteroid
testShipAsteroidCollision :: Test
testShipAsteroidCollision = TestCase $ do
    let ship = Ship {
            shipPos = (0, 0),
            shipDir = (0, 0),
            shipSpd = (0, 0),
            shipRot = 0,
            forward = False,
            shipHbx = (20, 20)
        }
    let asteroid = Asteroid {
            astPos = (0, 0),
            astDir = (0, 0),
            astSpd = (0, 0),
            astHbx = (20, 20),
            astSize = 20
        }
    assertEqual "Ship should collide with asteroid" True (collisionDetected (shipPos ship, shipHbx ship) (astPos asteroid, astHbx asteroid))

-- | Test if ship collides with asteroid
testShipAsteroidCollision2 :: Test
testShipAsteroidCollision2 = TestCase $ do
    let ship = Ship {
            shipPos = (0, 0),
            shipDir = (0, 0),
            shipSpd = (0, 0),
            shipRot = 0,
            forward = False,
            shipHbx = (20, 20)
        }
    let asteroid = Asteroid {
            astPos = (0, 0),
            astDir = (0, 0),
            astSpd = (0, 0),
            astHbx = (20, 20),
            astSize = 20
        }
    assertEqual "Ship should collide with asteroid" True (collisionDetected (shipPos ship, shipHbx ship) (astPos asteroid, astHbx asteroid))

-- | Test if ship does not collide with asteroid
testShipAsteroidCollision3 :: Test
testShipAsteroidCollision3 = TestCase $ do
    let ship = Ship {
            shipPos = (0, 0),
            shipDir = (0, 0),
            shipSpd = (0, 0),
            shipRot = 0,
            forward = False,
            shipHbx = (20, 20)
        }
    let asteroid = Asteroid {
            astPos = (1000, 1000),
            astDir = (0, 0),
            astSpd = (0, 0),
            astHbx = (20, 20),
            astSize = 20
        }
    assertEqual "Ship should not collide with asteroid" False (collisionDetected (shipPos ship, shipHbx ship) (astPos asteroid, astHbx asteroid))

--Give me a test to test my outofbounds function by putting an asteroid out of bounds  and making sure it despawns
testOutOfBounds :: Test
testOutOfBounds = TestCase $ do
    let asteroid = Asteroid {
            astPos = (1000, 1000),
            astDir = (0, 0),
            astSpd = (0, 0),
            astHbx = (20, 20),
            astSize = 20
        }
    assertEqual "Asteroid should be out of bounds" True (outOfBounds asteroid)

--Give me a test to test my outofbounds function by putting an asteroid in bounds and making sure it doesn't despawn
testOutOfBounds2 :: Test
testOutOfBounds2 = TestCase $ do
    let asteroid = Asteroid {
            astPos = (0, 0),
            astDir = (0, 0),
            astSpd = (0, 0),
            astHbx = (20, 20),
            astSize = 20
        }
    assertEqual "Asteroid should be in bounds" False (outOfBounds asteroid)



