module Main where

import           EventHandling  (onEvent)
import           GameLogic      (initialGame, playAsComputer)
import           Graphics.Gloss (play)
import           Rendering      (backgroundColor, draw, fps, window)

main :: IO ()
main =
  play
    window
    backgroundColor
    fps
    initialGame
    draw
    onEvent
    (const playAsComputer)
