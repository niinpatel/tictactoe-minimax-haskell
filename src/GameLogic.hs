module GameLogic where

import           Data.List
import           Data.Maybe
import           Utils      (replace)

type Position = (Int, Int)

type Cell = (Position, Player)

type Board = [Cell]

data Player
  = X
  | O
  | None
  deriving (Show, Eq)

data State
  = Running
  | GameOver Player
  deriving (Show)

data Game = Game
  { board        :: Board
  , playerToTurn :: Player
  , state        :: State
  } deriving (Show)

initialBoard :: Board
initialBoard =
  [(position, None) | position <- [(x, y) | x <- [0 .. 2], y <- [0 .. 2]]]

initialGame = Game {board = initialBoard, playerToTurn = X, state = Running}

winningMoves :: [[Position]]
winningMoves =
  [ [(0, 0), (0, 1), (0, 2)]
  , [(1, 0), (1, 1), (1, 2)]
  , [(2, 0), (2, 1), (2, 2)]
  , [(0, 0), (1, 0), (2, 0)]
  , [(0, 1), (1, 1), (2, 1)]
  , [(0, 2), (1, 2), (2, 2)]
  , [(0, 0), (1, 1), (2, 2)]
  , [(0, 2), (1, 1), (2, 0)]
  ]

makeMove :: Position -> Game -> Game
makeMove move game =
  game
    { board =
        replace ((== move) . fst) (\(x, _) -> (x, playerToTurn game)) $
        board game
    }

checkGameOver :: Game -> Game
checkGameOver game
  | playerHasWon = game {state = GameOver $ playerToTurn game}
  | itsATie = game {state = GameOver None}
  | otherwise = game
  where
    currentPlayer = playerToTurn game
    moves = map fst $ filter ((== currentPlayer) . snd) $ board game
    playerHasWon = any (all (`elem` moves)) winningMoves
    itsATie = all ((/= None) . snd) $ board game

flipPlayer game
  | currentPlayer == X = game {playerToTurn = O}
  | otherwise = game {playerToTurn = X}
  where
    currentPlayer = playerToTurn game

turn :: Position -> Game -> Game
turn move = flipPlayer . checkGameOver . makeMove move

g1 = turn (0, 0) initialGame

g2 = turn (1, 1) g1

g3 = turn (0, 1) g2

g4 = turn (2, 2) g3

g5 = turn (0, 2) g4
