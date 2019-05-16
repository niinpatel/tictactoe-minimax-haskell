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
drawPlayer player =
  case player of
    X ->
      color blue $
      pictures
        [ rotate 45 $ rectangleSolid width thickness
        , rotate 135 $ rectangleSolid width thickness
        ]
    O -> color red $ thickCircle (width / 2) thickness
    None -> blank
  where
    width = cellSize * 2 / 3
    thickness = cellSize / 30

drawDebugLog :: Game -> Picture
drawDebugLog = color white . text . debugLog

drawGameOverMessage :: Game -> Picture
drawGameOverMessage game =
  case state game of
    Running       -> blank
    GameOver X    -> drawBottomText 0.5 "X Wins!"
    GameOver O    -> drawBottomText 0.5 "O Wins!"
    GameOver None -> drawBottomText 0.5 "DRAW"

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
