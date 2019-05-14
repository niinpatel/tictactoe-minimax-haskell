module Rendering where

import           GameLogic
import           Graphics.Gloss

width, height, offset, fps :: Int
width = 450

height = 450

offset = 0

window :: Display
window = InWindow "Tic Tac Toe" (width, height) (offset, offset)

fps = 5

backgroundColor :: Color
backgroundColor = black

draw :: Game -> Picture
draw game = translate (-225) (225) $ pictures $ map drawCell gameBoard
  where
    gameBoard = board game

drawCell :: Cell -> Picture
drawCell cell =
  color (dark green) $
  translate x y $ pictures [rectangleWire 150 150, drawPlayer player]
  where
    ((i, j), player) = cell
    x = fromIntegral (i * 150) + 75
    y = fromIntegral (j * 150 * (-1)) - 75

drawPlayer :: Player -> Picture
drawPlayer X = color red $ circleSolid 20
drawPlayer O = color blue $ circleSolid 20
drawPlayer _ = Blank
