-- | This module defines how to turn the game state into a picture.
module View where

import Model
import Common
import Graphics.Gloss

import Graphics.UI.GLUT.Fonts

renderGame :: GameState -> IO Picture
renderGame = viewPure

-- temp function to determine width of a text.
centerText :: Picture -> IO Float
centerText (Text str) =
    fromIntegral <$> stringWidth Roman str
    --putStrLn $ "Width: " ++ show (fromIntegral width)

getWidth :: String -> IO Float
getWidth = centerText . Text

viewPure :: GameState -> IO Picture
viewPure GameOver = do
    width <- getWidth t
    return $ translate (-width / 2) 0 (color red (text t))
    where
        t = "Game Over!"
viewPure gstate
  | paused gstate = do
    width <- getWidth t
    return $ translate (-width / 4) 0 (scale 0.5 0.5 (color white (text t)))
  | otherwise = do
    let shipPicture = renderShip (ship gstate)
    --let shipPicture = draw (ship gstate)
    --let asteroidCountText = [renderText ("Asteroids: " ++ show (length (asteroids gstate))) (0) 0]
  
    let bulletPictures = map renderBullet (bullets gstate)
    let asteroidPictures = map renderAsteroid (asteroids gstate)
    let enemyPictures = map renderEnemy (enemies gstate)
    --return $ pictures (shipPicture : bulletPictures ++ asteroidPictures ++ enemyPictures ) --[var,var,var,]  -- TODO: Ship picure moet eigenlijk achteraan want nu tekenen andere dingen eroverheen.

    --return (pictures [draw (ship gstate), draw (asteroids gstate), draw (bullets gstate), draw (enemies gstate)])
    return (pictures [shipPicture, pictures bulletPictures, pictures asteroidPictures, pictures enemyPictures, color white (text (show (rotDir (ship gstate)))), color red (text (show (shipDir (ship gstate))))])
  where
    t  = "Game has been paused"
    


