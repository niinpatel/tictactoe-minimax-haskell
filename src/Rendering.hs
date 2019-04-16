module Rendering where

import           GameLogic
import           Graphics.Gloss

width, height, offset, fps :: Int
width = 400

height = 400

offset = 0

window :: Display
window = InWindow "Tic Tac Toe" (width, height) (offset, offset)

fps = 5

backgroundColor :: Color
backgroundColor = black

draw :: Game -> Picture
draw _ = color white $ circleSolid 100
