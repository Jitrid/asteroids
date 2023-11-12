-- -- lets save a gamestate to a file and load it back up
-- {-# LANGUAGE OverloadedStrings #-}

-- module JSONSAVER where
-- import qualified Data.ByteString.Lazy as B
-- import Data.Time.Clock.POSIX
-- import Data.Time.Clock
-- import Data.Time.Format
-- import Data.Aeson
-- import Model (GameState(..), initialState)


-- instance ToJSON GameState where
--     toJSON (Play ship asteroids enemies bullets score lives level time paused) =
--         object ["ship" .= ship, 
--                 "asteroids" .= asteroids,
--                 "enemies" .= enemies,
--                 "bullets" .= bullets,
--                 "score" .= score,
--                 "lives" .= lives,
--                 "level" .= level,
--                 "time" .= time,
--                 "paused" .= paused              
--                 ]
--     toJSON GameOver = object ["gameOver" .= True]

-- -- instance ToJSON GameState where
-- --     ToJSON (GameState ship bullets asteroids enemies score paused) =
-- --         object ["ship" .= ship, "bullets" .= bullets, "asteroids" .= asteroids, "enemies" .= enemies, "score" .= score, "paused" .= paused]

-- instance FromJSON GameState where
--     parseJSON = withObject "GameState" $ \v -> do
--         isGameOver <- v .:? "gameOver" .!= False
--         if isGameOver then return GameOver else
--             Play <$> v .: "ship"
--                  <*> v .: "asteroids"
--                  <*> v .: "enemies"
--                  <*> v .: "bullets"
--                  <*> v .: "score"
--                  <*> v .: "lives"
--                  <*> v .: "level"
--                  <*> v .: "time"
--                  <*> v .: "paused" 

-- saveGameState :: FilePath -> GameState -> IO ()
-- saveGameState path gstate = do 
--     currentTime <- getCurrentTime
--     let timeStamp = formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" currentTime
--     let gameStateWithTimeStamp = object ["timeStamp" .= timeStamp, "gameState" .= gstate]
--     B.writeFile path (encode gameStateWithTimeStamp)

-- loadGameState :: FilePath -> IO GameState
-- loadGameState path = do
--     result <- eitherDecode <$> B.readFile path
--     case result of
--         Left err -> do 
--             putStrLn err
--             return initialState
--         Right gstate -> return gstate


