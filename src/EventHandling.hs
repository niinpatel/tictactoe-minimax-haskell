module EventHandling where

import           Data.Maybe
import           GameLogic
import           Graphics.Gloss.Interface.Pure.Game

getMove :: Point -> Position
getMove (x, y) = (floor ((225 + x) / 150), floor ((225 - y) / 150))

onEvent :: Event -> Game -> Game
onEvent (EventKey (MouseButton LeftButton) Up _ mouse) = turn $ getMove mouse
onEvent (EventKey (Char 'r') _ _ _)                    = const initialGame
onEvent _                                              = id
