module Main where

import           GameLogic
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Rendering

main :: IO ()
main = play window backgroundColor fps g5 draw (const id) (const id)
