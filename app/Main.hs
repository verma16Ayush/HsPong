module Main where
import Graphics.Gloss


width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "HsPong" (width, height) (offset, offset)

backgroundColor :: Color
backgroundColor = makeColor 0.0 0.0 0.0 0.0

drawing :: Picture
drawing = pictures 
        [
            color (dark red) (circleSolid 30),
            color (light (light blue)) (rectangleSolid 10 50)
        ]

main :: IO ()
main = display window backgroundColor drawing
