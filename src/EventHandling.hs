module EventHandling where

import           GameLogic
import           Graphics.Gloss.Interface.Pure.Game
import           Rendering
import           Utils

getMove :: Point -> Position
getMove = mapPair floor . shrinkBy cellSize . shiftBy (cellSize * 1.5)

onEvent :: Event -> Game -> Game
onEvent (EventKey (MouseButton LeftButton) Up _ mouse) = turn $ getMove mouse
onEvent (EventKey (Char 'r') _ _ _)                    = const initialGame
onEvent _                                              = id
