module Solver(flagKnownMine, revealRandomCell, revealSafeCells, nothingCleared, flagMines121, boardWidth, boardLength, numMines) where

{-# LANGUAGE OverloadedStrings #-}
import Grid
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

boardLength, boardWidth, numMines :: Int
boardLength = 16
boardWidth = 30
numMines = 99




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

flagMines121 :: GameState -> GameState
flagMines121 gameState = 
  trace ("Flagging 121: " ++ show flaggedPositions) -- debug
  newState
  where
    flaggedPositions = apply121Rule (board gameState)
    newState = flagBoardCells gameState flaggedPositions

apply121Rule :: Board -> [Int]
apply121Rule board = concat [check121 pos board | pos <- [0..(rows * cols)-1]]
  where
    rows = length board
    cols = length (head board)

check121 :: Int -> Board -> [Int]
check121 index board =
  case getCell1d board index of
    Just cell | isRevealed cell && adjMines cell == 2 ->
      let
          validPairs = find121Patterns index board
          unrevealedTiles = concatMap (\(prev, next) -> findUnrevealedFromPair board (prev, next) index) validPairs
      in if not (null validPairs)
         then unrevealedTiles
         else []
    _ -> []


find121Patterns :: Int -> Board -> [(Int, Int)]
find121Patterns index board =
  let
      revealedNeighbors = getValidKnownNeighbours board index
      linearNeighbors = filter (isLinear index board) revealedNeighbors

      is121Pattern (prev, next) =
        isLinearTriple prev index next &&
        maybe False (\c -> adjMines c == 1) (getCell1d board prev) &&
        maybe False (\c -> adjMines c == 1) (getCell1d board next)
  in [(prev, next) | prev <- linearNeighbors, next <- linearNeighbors, prev /= next, is121Pattern (prev, next)]

isLinearTriple :: Int -> Int -> Int -> Bool
isLinearTriple i1 i2 i3 = 
  (sameRow i1 i2 && sameRow i2 i3) || (sameColumn i1 i2 && sameColumn i2 i3)

findUnrevealedFromPair :: Board -> (Int, Int) -> Int -> [Int]
findUnrevealedFromPair board (prev, next) middle
  | sameRow prev next = 
      filter (not . sameRow middle) $ hiddenCardinalNeighbours board prev ++ hiddenCardinalNeighbours board next
  | sameColumn prev next = 
      filter (not . sameColumn middle) $ hiddenCardinalNeighbours board prev ++ hiddenCardinalNeighbours board next
  | otherwise = [] 


isLinear :: Int -> Board -> Int -> Bool
isLinear index board neighbor = sameRow index neighbor || sameColumn index neighbor

sameRow :: Int -> Int -> Bool
sameRow i1 i2 = i1 `div` boardWidth == i2 `div` boardWidth

sameColumn :: Int -> Int -> Bool
sameColumn i1 i2 = i1 `mod` boardWidth == i2 `mod` boardWidth









