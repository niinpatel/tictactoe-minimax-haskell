module GameLogic where

import           Data.Function
import           Data.List
import           Utils         (replace)

type Position = (Int, Int)

data Player
  = X
  | O
  | None
  deriving (Show, Eq)

data Cell = Cell
  { position :: Position
  , player   :: Player
  } deriving (Show)

type Board = [Cell]

data State
  = Running
  | GameOver Player
  deriving (Show, Eq)

data Game = Game
  { board        :: Board
  , state        :: State
  , playerToTurn :: Player
  } deriving (Show)

initialBoard :: Board
initialBoard =
  [Cell {position = (i, j), player = None} | i <- [0 .. 2], j <- [0 .. 2]]

initialGame :: Game
initialGame = Game {board = initialBoard, state = Running, playerToTurn = X}

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
        replace
          ((== move) . position)
          (\cell -> cell {player = playerToTurn game}) $
        board game
    }

checkGameOver :: Game -> Game
checkGameOver game
  | playerHasWon = game {state = GameOver $ playerToTurn game}
  | itsATie = game {state = GameOver None}
  | otherwise = game
  where
    currentPlayer = playerToTurn game
    moves = map position $ filter ((== currentPlayer) . player) $ board game
    playerHasWon = any (all (`elem` moves)) winningMoves
    itsATie = all ((/= None) . player) $ board game

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

flipPlayer :: Game -> Game
flipPlayer game = game {playerToTurn = otherPlayer $ playerToTurn game}

turn :: Position -> Game -> Game
turn move game
  | isValidMove && isRunning game =
    (flipPlayer . checkGameOver . makeMove move) game
  | otherwise = game
  where
    isValidMove = elem move $ availableMoves game

availableMoves :: Game -> [Position]
availableMoves = map position . filter ((== None) . player) . board

isRunning :: Game -> Bool
isRunning game = state game == Running

playAsComputer :: Game -> Game
playAsComputer game
  | currentPlayer == O && isRunning game = turn bestMove game
  | otherwise = game
  where
    bestMove = pickBestMove game
    currentPlayer = playerToTurn game

pickBestMove :: Game -> Position
pickBestMove game =
  maximumBy (compare `on` (evaluateMove game $ playerToTurn game)) $
  availableMoves game

evaluateMove game maximizingPlayer move =
  case state nextGame of
    GameOver winner -> evaluateTerminalState winner
    Running         -> evaluateGameTree nextGame
  where
    nextGame = turn move game
    nextGameState = state nextGame
    evaluateTerminalState winner
      | winner == maximizingPlayer = 2
      | winner == None = 1
      | otherwise = 0
    evaluateGameTree game
      | playerToTurn game == maximizingPlayer = maximum nextGameTreeScores
      | otherwise = minimum nextGameTreeScores
      where
        nextGameTreeScores =
          map (evaluateMove game maximizingPlayer) $ availableMoves game
