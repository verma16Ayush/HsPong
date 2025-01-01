module Main where
import Graphics.Gloss
import Types.PongGame
import Graphics.Gloss.Data.ViewPort


width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "HsPong" (width, height) (offset, offset)

backgroundColor :: Color
backgroundColor = makeColor 0.0 0.0 0.0 0.0

initialState :: PongGame
initialState = PongGame {
    ballLoc = (0, 0),
    ballVelocity = (-40, 0),
    player1 = 0,
    player2 = 0,
    ballColor = dark red
}

render :: PongGame -> Picture

render pongGame = pictures [
                    ball pongGame,
                    walls,
                    mkPaddle (light blue) (-120) (player1 pongGame),
                    mkPaddle (light green) 120 (player2 pongGame)
                ]
                where
                    ball :: PongGame -> Picture
                    -- ballColor :: Color
                    ball game = uncurry translate (ballLoc game) (color (ballColor game) (circleSolid 10))
                    -- the bounding top and bottom walls
                    wallColor :: Color
                    -- takes a float of offset and returns a wall picture
                    wall :: Float -> Picture
                    wallColor = greyN 0.5
                    wall wallOffset =  translate 0 wallOffset (color wallColor (rectangleSolid 240 10))
                    wallTop = wall 150
                    wallBottom = wall (-150)
                    walls = pictures [wallTop, wallBottom]

                    -- make a paddle
                    mkPaddle :: Color -> Float -> Float -> Picture
                    mkPaddle col x y = translate x y (color col (rectangleSolid 20 50))


moveBall ::
  Float -> -- ^ time in seconds passed since the start of animation
  PongGame -> -- ^ initial game state
  PongGame -- ^ new game state

moveBall time game = game { ballLoc = (x_new, y_new)}
    where
        (x_old, y_old) = ballLoc game
        (vx, vy) = ballVelocity game
        x_new = x_old + vx * time
        y_new = y_old + vy * time

paddleBounce, wallBounce :: PongGame -> PongGame
type Radius = Float
type Position = (Float, Float)
wallCollision :: Radius -> PongGame -> Bool
paddleCollision :: Radius -> PongGame -> Bool

paddleCollision radius game = leftCollision || rightCollision
    where
        (x, y) = ballLoc game
        p1Height = player1 game
        p2Height = player2 game
        paddleWidth = 20

        paddleLeft = -(fromIntegral width / 2) + 2 * paddleWidth
        paddleRight = fromIntegral width / 2 - 2 * paddleWidth

        withinP1Height = y <= p1Height + 10 && y >= p1Height - 10
        withinP2Height = y <= p2Height + 10 && y >= p2Height - 10

        leftCollision = x - radius <= paddleLeft && withinP1Height
        rightCollision = x + radius >= paddleRight && withinP2Height

wallCollision radius game = topCollision || bottomCollision
    where
        (_, y) = ballLoc game
        topCollision = y + radius >= fromIntegral height / 2
        bottomCollision = y - radius <= - (fromIntegral height / 2)


wallBounce game = game {ballVelocity = (vx, vy') }
    where
        radius = 10
        -- old velocities
        (vx, vy) = ballVelocity game
        vy' = if wallCollision radius game
                then
                    -vy
                else
                    vy

paddleBounce game = game { ballVelocity = (vx', vy) }
    where
        radius = 10
        -- old velocities
        (vx, vy) = ballVelocity game
        vx' = if paddleCollision radius game
            then
                -vx
            else
                vx


fps :: Int
fps = 60


main :: IO ()
-- main = display window backgroundColor (render initialState)
main = simulate window backgroundColor fps initialState render update
    where
        update :: ViewPort -> Float -> PongGame -> PongGame
        update _ sec game = paddleBounce (wallBounce (moveBall sec game))
