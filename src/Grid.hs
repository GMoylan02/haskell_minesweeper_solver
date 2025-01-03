module Grid(Cell, GameState, Board, generateEmptyBoard, placeMines, positionsToBoard, 
insert, insert2d, cellToChar, printBoard, applyCountBombs, flagCell, revealCell, 
revealBoardCell, flagBoardCell, isGameOver, minesRemaining, isWinningBoard, initialiseGame, board, gameOver, isRevealed, isMine, isFlagged, adjMines)  where

{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Array
import Data.Maybe (fromMaybe)
import System.Random (randomRs, mkStdGen)
import Data.Time.Clock

data Cell = Cell
  { isMine    :: Bool
  , isRevealed :: Bool
  , isFlagged  :: Bool
  , adjMines   :: Int
  } deriving (Show, Eq)

type Board = [[Cell]]

data GameState = GameState
  { board :: Board
  , gameOver :: Bool
  , flaggedCount :: Int
  , minesCount :: Int
  } deriving (Show)

empty :: Cell
empty = Cell False False False 0

generateEmptyBoard :: Int -> Int -> Board
generateEmptyBoard length width = replicate length (replicate width Grid.empty)


placeMines :: Int -> Board -> IO Board
placeMines numMines board = do
  let len = length board
  let wid = length $ head board
  let totalCells = len * wid
  currTime <- getCurrentTime
  let timed = floor $ utctDayTime currTime :: Int
  let positions = take numMines . randomRs (0, totalCells - 1) . mkStdGen $ timed
  positionsToBoard positions board

positionsToBoard :: [Int] -> Board -> IO Board
positionsToBoard positions board = return $ foldl placeMine board positions
  where
    rows = length board
    cols = length (head board)
    indexToCoord idx = (idx `div` cols, idx `mod` cols)

    placeMine b idx = 
      let (r, c) = indexToCoord idx
          originalCell = (b !! r !! c) 
          updatedCell = originalCell {isMine = True} 
       in insert2d updatedCell r c b


insert :: a -> Int -> [a] -> [a]
insert x 0 []       = [x] 
insert x _ []       = error "Index out of bounds"
insert x 0 (_:xs)   = x : xs 
insert x n (z:zs)
  | n < 0           = error ("Negative index " ++ show n)
  | otherwise       = z : insert x (n-1) zs 

insert2d :: a -> Int -> Int -> [[a]] -> [[a]]
insert2d x 0 y []     = error ("Row index out of bounds at index " ++ show y)
insert2d x n y []     = error ("Row index out of bounds at index " ++ show y)
insert2d x 0 y (z:zs) = insert x y z : zs 
insert2d x n y (z:zs) = z : insert2d x (n-1) y zs 

applyCountBombs :: Board -> Board
applyCountBombs b = foldl updateBoard b [0 .. (rows * cols - 1)]
  where
    rows = length b
    cols = length (head b)

    updateBoard :: Board -> Int -> Board
    updateBoard board n
      | isMine (fromMaybe Grid.empty (getCell1d board n)) = board  
      | otherwise = updateCell board n (updatedCell (fromMaybe Grid.empty (getCell1d board n)))
      where
        updatedCell cell = cell { adjMines = countBombs board n }



--update nth cell in b to be c
updateCell :: Board -> Int -> Cell -> Board
updateCell b n c = insert2d c x y b
  where
    (x, y) = intToCoord b n


--count bombs at index n, assuming n isnt a bomb
countBombs :: Board -> Int -> Int
countBombs b n = count
  where 
    rows = length b
    cols = length (head b)
    (r, c) = (n `div` cols, n `mod` cols)
    cellTL = getCell1d b ((r-1) * cols + (c-1))
    cellT = getCell1d b ((r-1) * cols + c)
    cellTR = getCell1d b ((r-1) * cols + (c+1))
    cellL = getCell1d b (r * cols + (c-1))
    cellR = getCell1d b (r * cols + (c+1))
    cellBL = getCell1d b ((r+1) * cols + (c-1))
    cellB = getCell1d b ((r+1) * cols + c)
    cellBR = getCell1d b ((r+1) * cols + (c+1))

    isBomb cell = fromMaybe False (fmap isMine cell)
    count = sum (map (fromEnum . isBomb) [cellTL, cellT, cellTR, cellL, cellR, cellBL, cellB, cellBR])

isGameOver :: Board -> Bool
isGameOver b = any isRevealedAndMine cells
  where
    numPositions = getNumPositions b
    cells = [fromMaybe Grid.empty (getCell1d b i) | i <- [0..numPositions-1]]
    isRevealedAndMine cell = isRevealed cell && isMine cell

--winning board if all non-mine cells are revealed
--winning board if not any unrevealed tiles that arent mines
isWinningBoard :: Board -> Bool
isWinningBoard b = not $ any unrevealedAndNotMine cells
  where
    numPositions = getNumPositions b
    cells = [fromMaybe Grid.empty (getCell1d b i) | i <- [0..numPositions-1]]
    unrevealedAndNotMine cell = not (isRevealed cell || isMine cell) --equiv to (not (isRevealed cell)) && (not (isMine cell))

minesRemaining :: Board -> Int
minesRemaining b = length mines
  where 
    numPositions = getNumPositions b
    mines = filter (\cell -> isMine cell && not (isRevealed cell)) [fromMaybe Grid.empty (getCell1d b i) | i <- [0..numPositions-1]]

getNumPositions :: Board -> Int
getNumPositions b = x
  where
    rows = length b
    cols = length (head b)
    x = rows * cols


--if tile empty, recursively apply this to all adjacent empty tiles
revealBoardCell :: Int -> GameState -> GameState
revealBoardCell pos gameState =
  let
    currentBoard = board gameState
    currentCell = fromMaybe Grid.empty $ getCell1d currentBoard pos
    newCell = revealCell currentCell
    (r, c) = intToCoord currentBoard pos
    cols = length (head currentBoard)

    validNeighbors = filter (isValidPos currentBoard) 
      [ (r-1) * cols + (c-1),
        (r-1) * cols + c,
        (r-1) * cols + (c+1),
        r * cols + (c-1),
        r * cols + (c+1),
        (r+1) * cols + (c-1),
        (r+1) * cols + c,
        (r+1) * cols + (c+1)
      ]

    newBoard
      | not (isRevealed currentCell) && not (isMine newCell) && adjMines newCell == 0 =
          foldr (\neighborPos gState -> revealBoardCell neighborPos gState) (gameState { board = updateCell currentBoard pos newCell }) validNeighbors
      | otherwise = gameState { board = updateCell currentBoard pos newCell }

    isGameOver = isMine newCell && not (isFlagged newCell) -- Game over if a mine is revealed

  in
    newBoard { gameOver = gameOver newBoard || isGameOver }


initialiseGame :: Int -> Int -> Int -> IO GameState
initialiseGame rows cols numMines = do
  emptyBoard <- return $ generateEmptyBoard rows cols
  boardWithMines <- placeMines numMines emptyBoard
  let finalBoard = applyCountBombs boardWithMines
  return GameState {board = finalBoard, gameOver = False, flaggedCount = 0, minesCount = numMines}


--check if a position is within bounds
isValidPos :: Board -> Int -> Bool
isValidPos board pos =
  let rows = length board
      cols = length (head board)
  in pos >= 0 && pos < rows * cols


flagBoardCell :: Int -> GameState -> GameState
flagBoardCell n state = newState
  where
    b = board state
    numFlags = flaggedCount state
    currentCell = fromMaybe Grid.empty $ getCell1d b n
    newCell = flagCell currentCell
    newBoard = updateCell b n newCell
    newState = state {board = newBoard, flaggedCount = numFlags + 1}

flagCell :: Cell -> Cell
flagCell c 
  | isRevealed c = c
  | otherwise = c {isFlagged = True}

revealCell :: Cell -> Cell
revealCell c = c {isRevealed = True}

-- returns the cell at position x,y
getCell :: Board -> Int -> Int -> Maybe Cell
getCell b x y
  | x >= 0 && x < length b && y >= 0 && y < length (head b) = Just ((b !! x) !! y)
  | otherwise = Nothing

getCell1d :: Board -> Int -> Maybe Cell
getCell1d b n = getCell b x y
  where
    (x, y) = intToCoord b n

intToCoord :: Board -> Int -> (Int, Int)
intToCoord b n = x
  where
    rows = length b
    cols = length (head b)
    x = (n `div` cols, n `mod` cols)

-- debug functions
cellToChar :: Cell -> Char
cellToChar cell
  | isFlagged cell = 'F'
  | not (isRevealed cell) = '#'
  | isMine cell = '*'
  | adjMines cell > 0 = head (show (adjMines cell))
  | otherwise = '_'

rowToString :: [Cell] -> String
rowToString = map cellToChar

boardToStrings :: Board -> [String]
boardToStrings = map rowToString

printBoard :: Board -> IO ()
printBoard board = putStrLn $ unlines (boardToStrings board)