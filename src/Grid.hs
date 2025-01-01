module Grid(Cell, GameState, Board, generateEmptyBoard, placeMines, positionsToBoard, insert, insert2d, cellToChar, printBoard)  where

{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Array
import Data.Maybe (fromMaybe)
import System.Random (randomRs, mkStdGen)

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
  let positions = take numMines . randomRs (0, totalCells - 1) . mkStdGen $ 12
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


-- debug functions
cellToChar :: Cell -> Char
cellToChar cell
  | isFlagged cell = 'F'
  | not (isRevealed cell) = '#'
  | isMine cell = '*'
  | adjMines cell > 0 = head (show (adjMines cell))
  | otherwise = ' '

rowToString :: [Cell] -> String
rowToString = map cellToChar

boardToStrings :: Board -> [String]
boardToStrings = map rowToString

printBoard :: Board -> IO ()
printBoard board = putStrLn $ unlines (boardToStrings board)