module Main where

import           EventHandling
import           GameLogic
import           Graphics.Gloss
import           Rendering

main :: IO ()
main = play window backgroundColor fps initialGame draw onEvent (const id)
