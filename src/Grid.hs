module Grid(Cell, GameState, Board, generateEmptyBoard, placeMines, positionsToBoard, 
insert, insert2d, cellToChar, printBoard, applyCountBombs, flagCell, revealCell, 
revealBoardCell, flagBoardCell, isGameOver, minesRemaining, isWinningBoard, initialiseGame,
 board, gameOver, isRevealed, isMine, isFlagged, adjMines, Grid.empty, getCell1d, isValidPos, intToCoord, countNeighbourFlags,
 getValidNeighbours, isHidden, toggleFlagBoardCell, flagListOfPositions, flagHiddenNeighbours, hiddenNeighbours, flaggedNeighbours, revealHiddenNeighboursNotFlagged, probabilityCellIsMine)  where

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
  } deriving (Show, Eq)

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
  let timed = floor $ utctDayTime currTime :: Int --not the best but fine for now
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
  | n < 0           = error ("Negative index " ++ show n) --debug
  | otherwise       = z : insert x (n-1) zs 

insert2d :: a -> Int -> Int -> [[a]] -> [[a]]
insert2d x 0 y []     = error ("Row index out of bounds at index " ++ show y)
insert2d x n y []     = error ("Row index out of bounds at index " ++ show y)
insert2d x 0 y (z:zs) = insert x y z : zs 
insert2d x n y (z:zs) = z : insert2d x (n-1) y zs 

--given a board without adjacency counts, returns the same board with completed counts
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



--update nth cell in board b to be cell c
updateCell :: Board -> Int -> Cell -> Board
updateCell b n c = insert2d c x y b
  where
    (x, y) = intToCoord b n


--count bombs neighbouring 1d index n, assuming n isnt a bomb
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

--checks if game over
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

--counts number of mines (unused)
minesRemaining :: Board -> Int
minesRemaining b = length mines
  where 
    numPositions = getNumPositions b
    mines = filter (\cell -> isMine cell && not (isRevealed cell)) [fromMaybe Grid.empty (getCell1d b i) | i <- [0..numPositions-1]]

-- returns size of board
getNumPositions :: Board -> Int
getNumPositions b = x
  where
    rows = length b
    cols = length (head b)
    x = rows * cols

--if tile empty, recursively apply this to all adjacent empty tiles
revealBoardCell :: GameState -> Int -> GameState
revealBoardCell gameState pos =
  let
    currentBoard = board gameState
    currentCell = fromMaybe Grid.empty $ getCell1d currentBoard pos
    newCell = revealCell currentCell
    (r, c) = intToCoord currentBoard pos
    cols = length (head currentBoard)

    validNeighbors = getValidNeighbours currentBoard pos

    newBoard
      | not (isRevealed currentCell) && not (isMine newCell) && adjMines newCell == 0 =
          foldr (\neighborPos gState -> revealBoardCell gState neighborPos) 
                (gameState { board = updateCell currentBoard pos newCell }) 
                validNeighbors
      | otherwise = gameState { board = updateCell currentBoard pos newCell }

    isGameOver = isMine newCell && not (isFlagged newCell)

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


--toggles a flag at a 1d position, updates all relevant GameStates
toggleFlagBoardCell :: Int -> GameState -> GameState
toggleFlagBoardCell n state = newState
  where
    b = board state
    numFlags = flaggedCount state
    currentCell = fromMaybe Grid.empty $ getCell1d b n
    newCell = toggleFlagCell currentCell
    newBoard = updateCell b n newCell
    newState = state {board = newBoard, flaggedCount = numFlags + 1}

--toggles a cell flag
toggleFlagCell :: Cell -> Cell
toggleFlagCell c 
  | isRevealed c = c
  | otherwise = c {isFlagged = (not (isFlagged c))}

--ensures a cell is flagged if not revealed
flagCell :: Cell -> Cell
flagCell c
  | isRevealed c = c
  | otherwise = c {isFlagged = True}

isHidden :: Board -> Int -> Bool
isHidden b pos = not $ isRevealed cell
  where
    cell = fromMaybe Grid.empty $ getCell1d b pos

--counts number of flagged neighbours at a 1d coord
countNeighbourFlags :: Board -> Int -> Int
countNeighbourFlags b pos = length (filter isFlagged neighbours)
  where
    neighbours = map (fromMaybe Grid.empty . getCell1d b) (getValidNeighbours b pos)

--flags all cells given by a list of 1d coords, updates GameState accordingly
flagListOfPositions :: GameState -> [Int] -> GameState
flagListOfPositions gameState positions = foldl flagBoardCell gameState positions

--flags all hidden neighbours at a 1d coord, updates GameState accordingly
flagHiddenNeighbours :: GameState -> Int -> GameState
flagHiddenNeighbours gameState pos =
      let b = board gameState
          neighbours = hiddenNeighbours pos b
      in foldl flagBoardCell gameState neighbours

--gets list of hidden valid neighbours as list of 1d positions
hiddenNeighbours :: Int -> Board -> [Int]
hiddenNeighbours pos b = filter (isHidden b) (getValidNeighbours b pos)

--gets list of flagged valid neighbours as list of 1d positions
flaggedNeighbours :: Int -> Board -> [Int]
flaggedNeighbours pos b = filter (posIsFlagged b) (getValidNeighbours b pos)

--reveals all hidden valid neighbours that are not flagged
revealHiddenNeighboursNotFlagged :: GameState -> Int -> GameState
revealHiddenNeighboursNotFlagged gameState pos = 
  let b = board gameState
      neighbours = filter (posIsNotFlagged b) (hiddenNeighbours pos b)
  in foldl revealBoardCell gameState neighbours


--wrapper for isFlagged for use with positions rather than cells
posIsFlagged :: Board -> Int -> Bool
posIsFlagged b pos = isFlagged $ fromMaybe Grid.empty $ getCell1d b pos

--for convenience
posIsNotFlagged :: Board -> Int -> Bool
posIsNotFlagged b pos = not $ posIsFlagged b pos

--Wrapper to flag a board cell and update relevant counts accordingly
flagBoardCell :: GameState -> Int -> GameState
flagBoardCell state n = newState
  where
    b = board state
    numFlags = flaggedCount state
    currentCell = fromMaybe Grid.empty $ getCell1d b n
    newCell = flagCell currentCell
    newBoard = updateCell b n newCell
    newState = state {board = newBoard, flaggedCount = numFlags + 1}
    
--gets list of all valid neighbours as a list of 1d positions
getValidNeighbours :: Board -> Int -> [Int]
getValidNeighbours b pos = 
    let rows = length b
        cols = length (head b)
        (r, c) = intToCoord b pos
        neighbors = [(r-1, c-1), (r-1, c), (r-1, c+1),
                     (r,   c-1),           (r,   c+1),
                     (r+1, c-1), (r+1, c), (r+1, c+1)]
    in
      map (\(row, col) -> row * cols + col)
      $ filter (\(row, col) -> row >= 0 && row < rows && col >= 0 && col < cols)
               neighbors


-- naive way to calculate how likely a tile (denoted by 1d coord) is to be a mine
probabilityCellIsMine :: Board -> Int -> Double
probabilityCellIsMine b pos
  | numHiddenNeighbours == 0 = 0.0 
  | otherwise = (fromIntegral numMinesInAdjCells - fromIntegral numFlaggedNeighbours) / fromIntegral numHiddenNeighbours
  where
    neighbours = getValidNeighbours b pos
    numMinesInAdjCells = length $ filter isMine (map (\n -> fromMaybe Grid.empty (getCell1d b n)) neighbours)
    numFlaggedNeighbours = length $ filter (posIsFlagged b) neighbours
    numHiddenNeighbours = length $ filter (\n -> not (isRevealed (fromMaybe Grid.empty (getCell1d b n)))) neighbours


revealCell :: Cell -> Cell
revealCell c = c {isRevealed = True}

-- returns the cell at position x,y
getCell :: Board -> Int -> Int -> Maybe Cell
getCell b x y
  | x >= 0 && x < length b && y >= 0 && y < length (head b) = Just ((b !! x) !! y)
  | otherwise = Nothing

-- returns the cell at 1d coord 'n'
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


