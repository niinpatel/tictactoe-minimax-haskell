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
draw (Game board gameState _) =
  pictures [drawGameOverMessage gameState, drawCells board, instructions]

drawCells :: Board -> Picture
drawCells = pictures . map drawCell

drawCell :: Cell -> Picture
drawCell cell =
  color (dark green) $
  translate x y $ pictures [rectangleWire cellSize cellSize, drawPlayer player]
  where
    Cell position player = cell
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

drawGameOverMessage :: State -> Picture
drawGameOverMessage gameState =
  case gameState of
    Running       -> blank
    GameOver X    -> drawBottomText 0.5 "X Wins!"
    GameOver O    -> drawBottomText 0.5 "O Wins!"
    GameOver None -> drawBottomText 0.5 "DRAW"

translateToTop :: Picture -> Picture
translateToTop = translate (gridSize * (-0.5)) (gridSize * 0.5)

translateToBottom :: Picture -> Picture
translateToBottom = translate (gridSize * (-0.5)) (gridSize * (-0.5))

drawText :: String -> Picture
drawText = color white . text

drawTopText :: Float -> String -> Picture
drawTopText scalingFactor =
  translateToTop . scale scalingFactor scalingFactor . drawText

drawBottomText :: Float -> String -> Picture
drawBottomText scalingFactor =
  translateToBottom . scale scalingFactor scalingFactor . drawText

instructions :: Picture
instructions = drawTopText 0.25 "Press r to restart"

toPoint :: (Int, Int) -> Point
toPoint = mapPair fromIntegral
