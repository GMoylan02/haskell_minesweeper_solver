{-# Language ScopedTypeVariables #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny
import Grid
import Solver
import Data.IORef
import Control.Monad (forM_, when, forM, void)
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)


gameOngoing = True



main :: IO ()
main = do
  initialState <- initialiseGame boardLength boardWidth numMines
  startGUI defaultConfig $ setup initialState
  --startGUI defaultConfig $ setup debugState

setup :: GameState -> Window -> UI ()
setup initialState window = do
  gameStateRef <- liftIO $ newIORef initialState
  return window # set title "Minesweeper"

  flagModeState <- liftIO $ newIORef True
  flagButton <- UI.button #. "flagButton" # set UI.text "Flagmode Off" # set UI.id_ "flagButton"
  on UI.click flagButton $ \_ -> do
    currentState <- liftIO $ readIORef flagModeState
    let newState = not currentState
    liftIO $ writeIORef flagModeState newState
    updateToggleButton flagButton newState
    liftIO $ putStrLn "Flag Mode toggled"

  --button for debugging this feature
  flagMinesButton <- UI.button #. "flagAllMines" # set UI.text "[Debug] Flag all known mines" # set UI.id_ "flagAllMines"
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
  revealSafeCellsButton <- UI.button #. "revealSafeCells" # set UI.text "[Debug] Reveal all known safe cells" # set UI.id_ "revealSafeCells"
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

  revealRandomCellButton <- UI.button #. "revealRandomCell" # set UI.text "[Debug] Reveal a random cell" # set UI.id_ "revealRandomCell"
  on UI.click revealRandomCellButton $ \_ -> do
    gameState <- liftIO $ readIORef gameStateRef
    newState <- liftIO $ revealRandomCell gameState
    liftIO $ writeIORef gameStateRef newState
    let updatedGameOver = gameOver newState
    let victory = isWinningBoard $ board newState
    updateGrid newState
    when updatedGameOver $ updateStatus "Game Over!"
    when (not updatedGameOver && victory) $ updateStatus "You Win!"
    liftIO $ putStrLn "Random cell revealed"

  flag12xPattern <- UI.button #. "flag12xCells" # set UI.text "[Debug] Apply the 12x pattern" # set UI.id_ "flag12xCells"
  on UI.click flag12xPattern $ \_ -> do
    gameState <- liftIO $ readIORef gameStateRef
    let newState = flagMines12x gameState
    liftIO $ writeIORef gameStateRef newState
    let updatedGameOver = gameOver newState
    let victory = isWinningBoard $ board newState
    updateGrid newState
    when updatedGameOver $ updateStatus "Game Over!"
    when (not updatedGameOver && victory) $ updateStatus "You Win!"
    liftIO $ putStrLn "Applied 12x rule"

  solveButton <- UI.button #. "solve" # set UI.text "Ask algorithm to attempt to solve" # set UI.id_ "solve"
  on UI.click solveButton $ \_ -> do
    gameState <- liftIO $ readIORef gameStateRef
    newState <- solve gameState
    liftIO $ writeIORef gameStateRef newState
    let updatedGameOver = gameOver newState
    let victory = isWinningBoard $ board newState
    when updatedGameOver $ updateStatus "Game Over!"
    when (not updatedGameOver && victory) $ updateStatus "You Win!"
    liftIO $ putStrLn "Attempted solve"

  grid <- UI.div #. "grid"
  forM_ [0 .. boardLength - 1] $ \row -> do
    rowDiv <- UI.div #. "row"
    forM_ [0 .. boardWidth - 1] $ \col -> do
      let buttonId = "cell-" ++ show row ++ "-" ++ show col
      button <- UI.button #. "cell" # set UI.text "#" # set UI.id_ buttonId # set UI.style [ ("text-align", "center")
              , ("color", "darkgrey")
              , ("font-size", "25px")
              , ("width", "40px")
              , ("height", "40px")
              , ("line-height", "40px")
              , ("display", "inline-block")
              , ("border", "1px solid black")
              ]

      on UI.click button $ const $ handleCellClick gameStateRef flagModeState (row, col) button
      element rowDiv #+ [element button]
    element grid #+ [element rowDiv]

  status <- UI.div #. "status" # set UI.text "Game in progress..." # set UI.id_ "status"


  debugButtons <- UI.div #. "debug-buttons" #+ 
    [ element flagMinesButton
    , element revealSafeCellsButton
    , element revealRandomCellButton
    , element flag12xPattern
    ]

  getBody window #+ 
    [ element grid
    , element status
    , element flagButton
    , element solveButton
    , element debugButtons
    ]

  return ()


{-pseudocode implementation

while there hasnt been a decent initial clear:
    revealRandomCell;
flagKnownMine;
revealSafeCells;
while not gameOver && not isWinningBoard:
    while b changes:
        flagKnownMine
        revealSafeCells
        flagMines12X
    revealRandomCell
return gamestate

-}

solve :: GameState -> UI GameState
solve gameState = do
  newState <- handleNothingCleared gameState
  let minesFlagged = flagKnownMine newState
  updateGrid minesFlagged
  liftIO $ threadDelay 1000000
  let safeCellsRevealed = revealSafeCells minesFlagged
  updateGrid safeCellsRevealed
  result <- outerLoop safeCellsRevealed
  return result


handleNothingCleared :: GameState -> UI GameState
handleNothingCleared gameState | nothingCleared gameState = do
                                  liftIO $ threadDelay 1000000
                                  newState <- liftIO $ revealRandomCell gameState
                                  updateGrid newState
                                  handleNothingCleared newState
                                | otherwise = return gameState

outerLoop :: GameState -> UI GameState
outerLoop gameState | not ((isGameOver b) || isWinningBoard b) = do
                            newState <- innerLoop dummyState gameState
                            randomRevealed <- liftIO $ revealRandomCell newState
                            updateGrid randomRevealed
                            outerLoop randomRevealed
                    | otherwise = return gameState
                    where 
                      b = board gameState


innerLoop :: GameState -> GameState -> UI GameState
innerLoop initial newState | initial /= newState = do

                              liftIO $ threadDelay 1000000
                              let minesFlagged = flagKnownMine newState
                              updateGrid minesFlagged

                              liftIO $ threadDelay 1000000
                              let safeCellsRevealed = revealSafeCells minesFlagged
                              updateGrid safeCellsRevealed

                              liftIO $ threadDelay 1000000
                              let revealed121Cells = flagMines12x safeCellsRevealed
                              updateGrid revealed121Cells

                              innerLoop newState revealed121Cells
                            | otherwise = return newState


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
  liftIO $ putStrLn $ "Which is cell " ++ show (coordToInt b (row, col))
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
      let cellColour = cellToColour cell
      let buttonId = "cell-" ++ show row ++ "-" ++ show col
      button <- UI.getElementById window buttonId
      case button of
        Just b -> 
          element b 
            # set UI.text cellText 
            # set UI.style 
              [ ("text-align", "center")
              , ("color", cellColour)
              , ("font-size", "25px")
              , ("width", "40px")
              , ("height", "40px")
              , ("line-height", "40px")
              , ("display", "inline-block")
              , ("border", "1px solid black")
              ]
        Nothing -> do
           UI.span #. " " -- Return a placeholder element




updateStatus :: String -> UI ()
updateStatus msg = do
  liftIO $ putStrLn $ msg
  window <- askWindow
  status <- UI.getElementById window "status"
  case status of
    Just s -> void $ element s # set UI.text msg
    Nothing -> return () 
