module Main (main) where

import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
  ( Element,
    MonadIO (liftIO),
    UI,
    Window,
    column,
    defaultConfig,
    get,
    getBody,
    on,
    row,
    set,
    startGUI,
    title,
    (#),
    (#+),
  )
import Lib
    ( setPropertiesButton,
      buildButton,
      newGameButtonStyleStr,
      nextSign,
      blockButtonStyleStr,
      player1 )

setup :: Window -> UI ()
setup window = do
  _ <- return window # set title "Crosses-Noughts"

  mark <- liftIO $ newIORef player1

  buttonA1 <- buildButton
  buttonA2 <- buildButton
  buttonA3 <- buildButton
  buttonB1 <- buildButton
  buttonB2 <- buildButton
  buttonB3 <- buildButton
  buttonC1 <- buildButton
  buttonC2 <- buildButton
  buttonC3 <- buildButton

  display <-
    UI.button
      # set UI.text ("Turn " ++ player1)
      # set (UI.attr "type") "button"
      # set
        (UI.attr "style")
        "text-align: center; font-size: 50px; min-height: 75px; width: 450px; \
        \ margin-bottom: 50px; pointer-events : none;"

  newGame <-
    UI.button
      # set UI.text "New Game"
      # set UI.value "newGame"
      # set (UI.attr "type") "button"
      # set
        (UI.attr "style")
        (newGameButtonStyleStr "display: none;")

  let changeVisibilityNewGameButton :: Bool -> UI Element
      changeVisibilityNewGameButton bool = do
        let str = if bool then "" else "display: none;"
        pure newGame # set (UI.attr "style") (newGameButtonStyleStr str)

      gameButtonsLs = [buttonA1, buttonA2, buttonA3, buttonB1, buttonB2, buttonB3, buttonC1, buttonC2, buttonC3]
      buttonLs = display : newGame : gameButtonsLs
      winLineH1 = [buttonA1, buttonA2, buttonA3]
      winLineH2 = [buttonB1, buttonB2, buttonB3]
      winLineH3 = [buttonC1, buttonC2, buttonC3]
      winLineV1 = [buttonA1, buttonB1, buttonC1]
      winLineV2 = [buttonA2, buttonB2, buttonC2]
      winLineV3 = [buttonA3, buttonB3, buttonC3]
      winLineC1 = [buttonA1, buttonB2, buttonC3]
      winLineC2 = [buttonA3, buttonB2, buttonC1]
      winLines = [winLineH1, winLineH2, winLineH3, winLineV1, winLineV2, winLineV3, winLineC1, winLineC2]

      isWin :: String -> UI Bool
      isWin player = or <$> mapM (fmap (all (== player)) . mapM (get UI.value)) winLines

      isDraw = notElem "empty" <$> mapM (get UI.value) gameButtonsLs

      checkDraw :: [Char] -> UI ()
      checkDraw player = do
        bool <- isDraw
        void $
          if bool
            then pure display # set UI.text "Draw!" >> changeVisibilityNewGameButton True
            else pure display # set UI.text ("Turn " ++ nextSign player)

      checkGame :: String -> UI ()
      checkGame player = do
        bool <- isWin player
        if bool
          then do
            _ <- changeVisibilityNewGameButton True >> pure display # set UI.text (player ++ " Win!")
            sequence_ $ (# set (UI.attr "style") blockButtonStyleStr . pure) <$> gameButtonsLs
          else checkDraw player

      createRow = row . fmap pure
      rowA = createRow winLineH1
      rowB = createRow winLineH2
      rowC = createRow winLineH3

      gameBody =
        column [pure display, rowA, rowB, rowC, pure newGame]
          # set (UI.attr "style") "position: fixed; left: 30%; top : 10%;"

      clearField = do
        sequence_ $ setPropertiesButton . pure <$> gameButtonsLs
        liftIO . writeIORef mark $ player1
        _ <- changeVisibilityNewGameButton False
        void $ pure display # set UI.text ("Turn " ++ player1)

      clickOnButton button = on UI.click button $
        const $ do
          val <- get UI.value button
          case val of
            "newGame" -> clearField
            _ -> do
              sign <- liftIO $ readIORef mark
              _ <- liftIO . writeIORef mark . nextSign $ sign
              _ <-
                pure button # set UI.text sign
                  # set UI.value sign
                  # set (UI.attr "style") blockButtonStyleStr
              valLs <- sequence $ get UI.value <$> gameButtonsLs -- delete
              liftIO $ print valLs -- delete
              checkGame sign

  _ <- getBody window #+ [gameBody]

  void . sequence $ clickOnButton <$> buttonLs

main :: IO ()
main = startGUI defaultConfig setup
