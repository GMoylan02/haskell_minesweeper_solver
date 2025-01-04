{-# Language ScopedTypeVariables #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny
import Grid
import Solver
import Data.IORef
import Control.Monad (forM_, when, forM, void)
import Data.Maybe (fromMaybe)

rows, cols, numMines :: Int
rows = 10
cols = 10
numMines = 15

gameOngoing = True

main :: IO ()
main = do
  initialState <- initialiseGame rows cols numMines
  startGUI defaultConfig $ setup initialState

setup :: GameState -> Window -> UI ()
setup initialState window = do
  gameStateRef <- liftIO $ newIORef initialState
  return window # set title "MineSweeper"

  flagModeState <- liftIO $ newIORef True
  flagButton <- UI.button #. "flagButton" # set UI.text "Flagmode Off" # set UI.id_ "flagButton"
  on UI.click flagButton $ \_ -> do
    currentState <- liftIO $ readIORef flagModeState
    let newState = not currentState
    liftIO $ writeIORef flagModeState newState
    updateToggleButton flagButton newState
    liftIO $ putStrLn "Flag Mode toggled"

  --button for debugging this feature
  flagMinesButton <- UI.button #. "flagAllMines" # set UI.text "Flag all known mines" # set UI.id_ "flagAllMines"
  on UI.click flagMinesButton $ \_ -> do
    gameState <- liftIO $ readIORef gameStateRef
    let newState = flagKnownMine gameState
    liftIO $ writeIORef gameStateRef newState
    let updatedGameOver = gameOver newState
    let victory = isWinningBoard $ board newState
    updateGrid newState
    when updatedGameOver $ updateStatus "Game Over!"
    when (not updatedGameOver && victory) $ updateStatus "You Win!"
    liftIO $ putStrLn "all mines flagged"

  --button for debugging this feature
  revealSafeCellsButton <- UI.button #. "revealSafeCells" # set UI.text "Reveal all known safe cells" # set UI.id_ "revealSafeCells"
  on UI.click revealSafeCellsButton $ \_ -> do
    gameState <- liftIO $ readIORef gameStateRef
    let newState = revealSafeCells gameState
    liftIO $ writeIORef gameStateRef newState
    let updatedGameOver = gameOver newState
    let victory = isWinningBoard $ board newState
    updateGrid newState
    when updatedGameOver $ updateStatus "Game Over!"
    when (not updatedGameOver && victory) $ updateStatus "You Win!"
    liftIO $ putStrLn "All safe cells revealed"

  revealSafestCellButton <- UI.button #. "revealSafestCell" # set UI.text "Reveal the cell with lowest mine probability" # set UI.id_ "revealSafestCell"
  on UI.click revealSafestCellButton $ \_ -> do
    gameState <- liftIO $ readIORef gameStateRef
    let newState = revealSafestCell gameState
    liftIO $ writeIORef gameStateRef newState
    let updatedGameOver = gameOver newState
    let victory = isWinningBoard $ board newState
    updateGrid newState
    when updatedGameOver $ updateStatus "Game Over!"
    when (not updatedGameOver && victory) $ updateStatus "You Win!"
    liftIO $ putStrLn "Safest cell revealed"

  grid <- UI.div #. "grid"
  forM_ [0 .. rows - 1] $ \row -> do
    rowDiv <- UI.div #. "row"
    forM_ [0 .. cols - 1] $ \col -> do
      let buttonId = "cell-" ++ show row ++ "-" ++ show col
      button <- UI.button #. "cell" # set UI.text "#" # set UI.id_ buttonId -- Add ID here
      on UI.click button $ const $ handleCellClick gameStateRef flagModeState (row, col) button
      element rowDiv #+ [element button]
    element grid #+ [element rowDiv]

  status <- UI.div #. "status" # set UI.text "Game in progress..." # set UI.id_ "status"


  getBody window #+ [element grid, element status, element flagButton, element flagMinesButton, element revealSafeCellsButton, element revealSafestCellButton]
  return ()

updateToggleButton :: Element -> Bool -> UI ()
updateToggleButton button state = do
  let (buttonText, buttonClass) = if state
        then ("Flagmode off", "toggle-button-off")
        else ("Flagmode on", "toggle-button-on")
  element button # set UI.text buttonText # set UI.class_ buttonClass
  return ()


handleCellClick :: IORef GameState -> IORef Bool -> (Int, Int) -> Element -> UI ()
handleCellClick gameStateRef flagModeRef (row, col) button = do
  liftIO $ putStrLn $ "Clicked on: (" ++ show row ++ ", " ++ show col ++ ")"
  isFlagMode <- liftIO $ readIORef flagModeRef
  gameState <- liftIO $ readIORef gameStateRef
  when (gameOver gameState) $ return ()
  let b = board gameState
  liftIO $ putStrLn $ "This tile is " ++ show (fromMaybe Grid.empty $ getCell b row col)
  let cols = length (head b)
  let pos = row * cols + col
  let newGameState = if isFlagMode 
        then (revealBoardCell gameState pos)
        else (toggleFlagBoardCell pos gameState)
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
        Nothing -> UI.span #. " "


updateStatus :: String -> UI ()
updateStatus msg = do
  liftIO $ putStrLn $ msg
  window <- askWindow
  status <- UI.getElementById window "status"
  case status of
    Just s -> void $ element s # set UI.text msg
    Nothing -> return () 
