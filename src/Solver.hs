module Solver(flagKnownMine, revealRandomCell, revealSafeCells, revealSafestCell, nothingCleared) where

{-# LANGUAGE OverloadedStrings #-}
import Grid
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)




revealRandomCell :: GameState -> IO GameState
revealRandomCell gameState = do
    let b = board gameState
    let rows = length b
    let cols = length (head b)
    let hiddenCells = [pos | pos <- [0..(rows * cols) - 1],
                            let cell = fromMaybe Grid.empty (getCell1d b pos),
                            not (isRevealed cell),
                            not (isFlagged cell)]
    if null hiddenCells
        then return gameState
        else do
            randomIndex <- randomRIO (0, length hiddenCells - 1)
            return $ revealBoardCell gameState (hiddenCells !! randomIndex)



--forall revealed cells with number n, if no.hidden neighbours == n, flag all hidden neighbours
flagKnownMine :: GameState -> GameState
flagKnownMine gameState =
  trace ("Flagging cells: " ++ show cellsThatNeighbourMines ++
         "\nHidden neighbors: " ++ show (map (\p -> (p, hiddenNeighbours p b)) cellsThatNeighbourMines)) -- debug
  newState
  where
    b = board gameState
    rows = length b
    cols = length (head b)

    cellsThatNeighbourMines =
      [ pos
      | pos <- [0..(rows * cols) - 1],
        let cell = fromMaybe Grid.empty (getCell1d b pos),
        isRevealed cell,
        adjMines cell /= 0,
        let flaggedCount = countNeighbourFlags b pos,
        let neighbours = hiddenNeighboursNotFlagged pos b,
        length neighbours + flaggedCount == adjMines cell
      ]

    newState = foldl (\state pos ->
                        foldl flagBoardCell state (hiddenNeighbours pos b))
                     gameState
                     cellsThatNeighbourMines



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
revealSafestCell gameState = 
    if null sortedCellsBySafety 
    then gameState
    else revealBoardCell gameState (head sortedCellsBySafety)
  where
    b = board gameState
    rows = length b
    cols = length (head b)
    validCells = [pos 
                 | pos <- [0..(rows * cols) - 1],
                   let cell = fromMaybe Grid.empty (getCell1d b pos),
                   not (isRevealed cell),
                   not (isFlagged cell)]
    sortedCellsBySafety = 
        trace debugOutput $
        sortBy (comparing (probabilityCellIsMine b)) validCells
    debugOutput = 
        unlines [ "Valid cells: " ++ show validCells,
                  "Cells with their safety value: " ++ show [(pos, probabilityCellIsMine b pos) | pos <- validCells]
                ]







boardIsDefault :: GameState -> Bool
boardIsDefault gameState = all isCellDefault (concat b)
  where
    b = board gameState
    isCellDefault :: Cell -> Bool
    isCellDefault cell = not (isRevealed cell) && not (isFlagged cell)


nothingCleared :: GameState -> Bool
nothingCleared gameState = not (any isCleared (concat b))
  where
    b = board gameState  
    isCleared cell = isRevealed cell && adjMines cell == 0

