module Rendering where

import           GameLogic
import           Graphics.Gloss
import           Utils

cellSize, gridSize :: Float
gridSize = 450

cellSize = gridSize / 3

windowWidth, windowHeight, offset, fps :: Int
offset = 100

fps = 5

windowWidth = floor gridSize

windowHeight = floor gridSize + offset

window :: Display
window = InWindow "Tic Tac Toe" (windowWidth, windowHeight) (offset, offset)

backgroundColor :: Color
backgroundColor = black

draw :: Game -> Picture
draw game = pictures [drawGameOverMessage game, drawCells game, instructions]

drawCells :: Game -> Picture
drawCells = pictures . map drawCell . board

drawCell :: Cell -> Picture
drawCell cell =
  color (dark green) $
  translate x y $ pictures [rectangleWire cellSize cellSize, drawPlayer player]
  where
    (position, player) = cell
    (x, y) = shiftBy (-cellSize) $ scaleBy cellSize $ toPoint position

drawPlayer :: Player -> Picture
drawPlayer X = color red $ circleSolid 20
drawPlayer O = color blue $ circleSolid 20
drawPlayer _ = blank

drawDebugLog :: Game -> Picture
drawDebugLog = color white . text . debugLog

drawGameOverMessage :: Game -> Picture
drawGameOverMessage game =
  case state game of
    Running -> blank
    GameOver winner ->
      drawBottomText
        0.5
        (if (winner == None)
           then "DRAW"
           else show winner ++ " wins!")

translateToTop :: Float -> Picture -> Picture
translateToTop scalingFactor =
  translate (-gridSize / scalingFactor / 3) (gridSize / scalingFactor / 2)

translateToBottom :: Float -> Picture -> Picture
translateToBottom scalingFactor =
  translate (-gridSize / scalingFactor / 3) (-gridSize / scalingFactor / 2)

drawText :: String -> Picture
drawText = color white . text

drawTopText :: Float -> String -> Picture
drawTopText scalingFactor =
  scale scalingFactor scalingFactor . translateToTop scalingFactor . drawText

drawBottomText :: Float -> String -> Picture
drawBottomText scalingFactor =
  scale scalingFactor scalingFactor . translateToBottom scalingFactor . drawText

instructions :: Picture
instructions = drawTopText 0.25 "Press r to restart"

toPoint :: (Int, Int) -> Point
toPoint = mapPair fromIntegral
