{-# Language ScopedTypeVariables #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny
import Grid
import Data.IORef
import Control.Monad (forM_, when, forM, void)

rows, cols, numMines :: Int
rows = 10
cols = 10
numMines = 15

main :: IO ()
main = do
  initialState <- initialiseGame rows cols numMines
  startGUI defaultConfig $ setup initialState

setup :: GameState -> Window -> UI ()
setup initialState window = do
  gameStateRef <- liftIO $ newIORef initialState
  return window # set title "MineSweeper"

  grid <- UI.div #. "grid"
  forM_ [0 .. rows - 1] $ \row -> do
    rowDiv <- UI.div #. "row"
    forM_ [0 .. cols - 1] $ \col -> do
      let buttonId = "cell-" ++ show row ++ "-" ++ show col
      button <- UI.button #. "cell" # set UI.text "#" # set UI.id_ buttonId -- Add ID here
      on UI.click button $ const $ handleCellClick gameStateRef (row, col) button
      element rowDiv #+ [element button]
    element grid #+ [element rowDiv]

  status <- UI.div #. "status" # set UI.text "Game in progress..."
  getBody window #+ [element grid, element status]
  return ()


handleCellClick :: IORef GameState -> (Int, Int) -> Element -> UI ()
handleCellClick gameStateRef (row, col) button = do
  liftIO $ putStrLn $ "Clicked on: (" ++ show row ++ ", " ++ show col ++ ")" -- Debug message
  gameState <- liftIO $ readIORef gameStateRef
  when (gameOver gameState) $ return ()
  let b = board gameState
  let cols = length (head b)
  let pos = row * cols + col
  let newGameState = revealBoardCell pos gameState
  liftIO $ writeIORef gameStateRef newGameState

  let updatedGameOver = gameOver newGameState
  let victory = isWinningBoard $ board newGameState

  updateGrid newGameState
  when updatedGameOver $ updateStatus "Game Over!"
  when (not updatedGameOver && victory) $ updateStatus "You Win!"


updateGrid :: GameState -> UI ()
updateGrid gameState = do
  let b = board gameState
  window <- askWindow
  forM_ (zip [0 ..] b) $ \(row, cells) ->
    forM_ (zip [0 ..] cells) $ \(col, cell) -> do
      let cellText = [cellToChar cell]
      let cellClass = if isRevealed cell then "cell revealed" else "cell hidden"
      let buttonId = "cell-" ++ show row ++ "-" ++ show col
      button <- UI.getElementById window buttonId
      case button of
        Just b -> element b # set UI.text cellText # set UI.class_ cellClass
        Nothing -> UI.span #. "hidden-placeholder"


updateStatus :: String -> UI ()
updateStatus msg = do
  window <- askWindow
  status <- UI.getElementById window "status"
  case status of
    Just s -> void $ element s # set UI.text msg
    Nothing -> return () 
