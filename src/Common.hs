module Common where

-- Data types to assist with readability and maintainability of the code base.
type Point      = (Float,Float)
type Direction  = (Float,Float)
type Velocity   = (Float,Float)
type HitboxUnit = (Float,Float)
data Difficulty = Easy | Normal | Hard | Extreme deriving (Eq, Show)
