module Types.PongGame (PongGame(..)) where

import Graphics.Gloss

data PongGame = PongGame {
    ballLoc :: (Float, Float),
    ballVelocity :: (Float, Float),
    -- player paddle height, 0 is center
    player1 :: Float,
    player2 :: Float,
    ballColor :: Color
} deriving (Show)