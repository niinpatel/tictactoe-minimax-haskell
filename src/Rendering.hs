module Rendering where

import           GameLogic
import           Graphics.Gloss
import           Utils

windowSize, offset, fps :: Int
windowSize = 450

offset = 0

fps = 5

cellSize :: Num a => a
cellSize = fromIntegral (windowSize `div` 3)

window :: Display
window = InWindow "Tic Tac Toe" (windowSize, windowSize) (offset, offset)

backgroundColor :: Color
backgroundColor = black

draw :: Game -> Picture
draw game =
  pictures [drawGameOverMessage game, drawDebugLog game, drawCells game]

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
      pictures
        [ translate (-225) 0 $ color white $ text $ show winner ++ " wins!"
        , translate (-225) (-50) $ color white $ text $ "press r to restart"
        ]

toPoint :: (Int, Int) -> Point
toPoint = mapPair fromIntegral
