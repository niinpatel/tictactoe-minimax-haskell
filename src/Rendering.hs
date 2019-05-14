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
draw game =
  pictures [drawGameOverMessage game, drawDebugLog game, drawCells game]

drawCells :: Game -> Picture
drawCells game = translate (-225) 225 $ pictures $ map drawCell $ board game

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
drawPlayer _ = blank

drawDebugLog :: Game -> Picture
drawDebugLog game = translate (-225) 225 $ color white $ text $ debugLog game

drawGameOverMessage :: Game -> Picture
drawGameOverMessage game =
  case state game of
    Running -> blank
    GameOver winner ->
      pictures
        [ translate (-225) 0 $ color white $ text $ show winner ++ " wins!"
        , translate (-225) (-50) $ color white $ text $ "press r to restart"
        ]
