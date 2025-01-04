module Solver() where

{-# LANGUAGE OverloadedStrings #-}
import Grid
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)

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
            return $ revealBoardCell randomIndex gameState


--forall revealed cells with number n, if no.hidden neighbours == n, flag all hidden neighbours
flagKnownMine :: GameState -> GameState
flagKnownMine gameState = newState
    where
        b = board gameState
        rows = length b
        cols = length (head b)
        mines = [pos | pos <- [0..(rows * cols) - 1], 
                 let cell = fromMaybe Grid.empty (getCell1d b pos), 
                 isRevealed cell, 
                 let neighbours = hiddenNeighbours pos b,
                 length neighbours == adjMines cell - countNeighbourFlags b pos]
        newState = flagListOfPositions gameState mines
        

hiddenNeighbours :: Int -> Board -> [Int]
hiddenNeighbours pos b = filter (isHidden b) (getValidNeighbours b pos)

        
--For any revealed cell with a number n, if the number of hidden neighbors equals n, all hidden neighbors are mines. Flag them.

{-pseudocode implementation
initial move;
while game isnt over:
    store initial board in variable
    while no change:
        identify known mines
return gamestate
-}