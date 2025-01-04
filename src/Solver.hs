module Solver(flagKnownMine, revealRandomCell, revealSafeCells, revealSafestCell) where

{-# LANGUAGE OverloadedStrings #-}
import Grid
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)


revealRandomCell :: GameState -> IO GameState
revealRandomCell gameState = do
    let b = board gameState
    let rows = length b
    let cols = length (head b)
    let hiddenCells = [pos | pos <- [0..(rows*cols)-1], not (isRevealed (fromMaybe Grid.empty (getCell1d b pos)))]
    if null hiddenCells
        then return gameState
        else do
            randomIndex <- randomRIO (0, length hiddenCells - 1)
            return $ revealBoardCell gameState randomIndex


--forall revealed cells with number n, if no.hidden neighbours == n, flag all hidden neighbours
    --this is bugged
flagKnownMine :: GameState -> GameState
flagKnownMine gameState = newState
    where
        b = board gameState
        rows = length b
        cols = length (head b)
        cellsThatNeighbourMines = [pos | pos <- [0..(rows * cols) - 1], 
                 let cell = fromMaybe Grid.empty (getCell1d b pos), 
                 isRevealed cell, 
                 let neighbours = hiddenNeighbours pos b,
                 length neighbours == adjMines cell - countNeighbourFlags b pos]
        newState = foldl flagHiddenNeighbours gameState cellsThatNeighbourMines

--for all revealed cells with number n, if no. flagged neighbours == n, reveal all remaining hidden neighbours
    
revealSafeCells :: GameState -> GameState 
revealSafeCells gameState = newState
    where
        b = board gameState
        rows = length b
        cols = length (head b)
        safeCells = [pos | pos <- [0..(rows * cols) - 1], 
                 let cell = fromMaybe Grid.empty (getCell1d b pos), 
                 isRevealed cell, 
                 let neighbours = flaggedNeighbours pos b,
                 length neighbours == adjMines cell]
        newState = foldl revealHiddenNeighboursNotFlagged gameState safeCells

                 
revealSafestCell :: GameState -> GameState
revealSafestCell gameState = newState
    where
        b = board gameState
        rows = length b
        cols = length (head b)
        sortedCellsBySafety = sortBy (comparing (negate . probabilityCellIsMine b)) [pos | pos <- [0..(rows * cols) - 1],
                let cell = fromMaybe Grid.empty (getCell1d b pos),
                not (isRevealed cell),
                not (isFlagged cell)]
        newState = 
            if null sortedCellsBySafety then gameState
            else revealBoardCell gameState $ head sortedCellsBySafety


{-pseudocode implementation
initial move;
while game isnt over:
    store initial board in variable
    while no change:
        identify known mines
return gamestate
-}