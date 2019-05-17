module GameLogic where

import           Data.Function
import           Data.List
import           Utils         (replace)

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
  deriving (Show, Eq)

data Game = Game
  { board        :: Board
  , playerToTurn :: Player
  , state        :: State
  , debugLog     :: String
  } deriving (Show)

initialBoard :: Board
initialBoard =
  [(position, None) | position <- [(x, y) | x <- [0 .. 2], y <- [0 .. 2]]]

initialGame =
  Game
    { board = initialBoard
    , playerToTurn = X
    , state = Running
    , debugLog = "debug info"
    }

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
availableMoves = map fst . filter ((== None) . snd) . board

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
  maximumBy (compare `on` (evaluateMove game)) $ availableMoves game

evaluateMove :: Game -> Position -> Int
evaluateMove game move =
  case state nextGame of
    GameOver O    -> 2
    GameOver None -> 1
    GameOver X    -> 0
    Running       -> evaluateGameTree nextGame
  where
    nextGame = turn move game
    evaluateGameTree game =
      case playerToTurn game of
        X -> minimum nextGameTreeScores
        O -> maximum nextGameTreeScores
      where
        nextGameTreeScores = map (evaluateMove game) $ availableMoves game
