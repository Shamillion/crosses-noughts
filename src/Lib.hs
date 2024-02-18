module Lib where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
  ( Element,
    UI,
    set,
    (#),
  )

data Player = Player1 | Player2

instance Show Player where
  show Player1 = "X"
  show Player2 = "O"

player1 :: String
player1 = show Player1

player2 :: String
player2 = show Player2

createButton :: UI Element
createButton = UI.button

setPropertiesButton :: UI Element -> UI Element
setPropertiesButton btn =
  btn
    # set UI.text "."
    # set UI.value "empty"
    # set (UI.attr "type") "button"
    # set
      (UI.attr "style")
      "min-width: 150px; min-height: 150px; font-size: 100px; \
      \ color: rgba(0, 0, 0, 0); pointer-events : auto;"

buildButton :: UI Element
buildButton = setPropertiesButton createButton

newGameButtonStyleStr :: [Char] -> [Char]
newGameButtonStyleStr str =
  "text-align: center; font-size: 50px; min-height: 75px; \
  \ width: 450px; margin-top: 50px; "
    ++ str

nextSign :: [Char] -> [Char]
nextSign sign
  | sign == player1 = player2
  | otherwise = player1

blockButtonStyleStr :: [Char]
blockButtonStyleStr = "min-width: 150px; min-height: 150px; font-size: 100px; pointer-events: none;"
