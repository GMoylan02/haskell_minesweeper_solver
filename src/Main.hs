{-# Language ScopedTypeVariables #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny
import Grid

calculatorSize = 400

main :: IO ()
main = do
  putStrLn "hello"

{-
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  return window # set title "Calculator"
  

  calculator <- UI.canvas
    # set UI.height calculatorSize
    # set UI.width calculatorSize
    # set UI.style [("border", "solid black 1px"), ("background", "#eee")]
  

-- Create buttons for the numbers and basic operations
  one <- UI.button #+ [string "1"]
  two <- UI.button #+ [string "2"]
  three <- UI.button #+ [string "3"]
  four <- UI.button #+ [string "4"]
  five <- UI.button #+ [string "5"]
  six <- UI.button #+ [string "6"]
  seven <- UI.button #+ [string "7"]
  eight <- UI.button #+ [string "8"]
  nine <- UI.button #+ [string "9"]
  zero <- UI.button #+ [string "0"]
  plus <- UI.button #+ [string "+"]
  minus <- UI.button #+ [string "-"]
  multiply <- UI.button #+ [string "*"]
  divide <- UI.button #+ [string "/"]
  equals <- UI.button #+ [string "="]
  clear <- UI.button #+ [string "C"]

  -- Input field to display calculations
  display <- UI.input
    # set UI.style [("width", "90%"), ("margin", "10px auto"), ("text-align", "right")]
    # set (UI.attr "readonly") "true"

  on UI.click one $ \_ -> appendText "1" display

  -- Organize buttons into a grid layout
  let buttonGrid :: [[UI Element]]
      buttonGrid = [[element seven, element eight, element nine, element divide],
                    [element four, element five, element six, element multiply],
                    [element one, element two, element three, element minus],
                    [element clear, element zero, element equals, element plus]]


  grid <- UI.div
    #+ map (\row -> UI.div #. "row" #+ map (withClass "button") row) buttonGrid

  getBody window #+ [element display, element grid]


  

  return ()

withClass :: String -> UI Element -> UI Element
withClass cls el = el # set UI.class_ cls

appendText :: String -> UI Element -> UI ()
appendText newText inputField = do
  currentText <- get UI.value inputField
  inputField # set UI.value (currentText ++ newText)

-}

