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
  ( blockButtonStyleStr,
    buildButton,
    newGameButtonStyleStr,
    nextPlayer,
    player1,
    setPropertiesButton,
  )

setup :: Window -> UI ()
setup window = do
  _ <- return window # set title "Crosses-Noughts"

  turn <- liftIO $ newIORef player1

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

  newGameButton <-
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
        pure newGameButton # set (UI.attr "style") (newGameButtonStyleStr str)

      gameButtonsLs = [buttonA1, buttonA2, buttonA3, buttonB1, buttonB2, buttonB3, buttonC1, buttonC2, buttonC3]
      buttonLs = display : newGameButton : gameButtonsLs
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
            else pure display # set UI.text ("Turn " ++ nextPlayer player)

      checkGame :: String -> UI ()
      checkGame player = do
        bool <- isWin player
        if bool
          then do
            _ <- changeVisibilityNewGameButton True >> pure display # set UI.text (player ++ " Win!")
            sequence_ $ (# set (UI.attr "style") blockButtonStyleStr . pure) <$> gameButtonsLs
          else checkDraw player

      createRow :: [Element] -> UI Element
      createRow = row . fmap pure

      rowA = createRow winLineH1
      rowB = createRow winLineH2
      rowC = createRow winLineH3

      gameBody =
        column [pure display, rowA, rowB, rowC, pure newGameButton]
          # set (UI.attr "style") "position: fixed; left: 30%; top : 10%;"

      clearField = do
        sequence_ $ setPropertiesButton . pure <$> gameButtonsLs
        liftIO . writeIORef turn $ player1
        _ <- changeVisibilityNewGameButton False
        void $ pure display # set UI.text ("Turn " ++ player1)

      clickOnButton :: Element -> UI ()
      clickOnButton button = on UI.click button $
        const $ do
          val <- get UI.value button
          case val of
            "newGame" -> clearField
            _ -> do
              player <- liftIO $ readIORef turn
              _ <- liftIO . writeIORef turn . nextPlayer $ player
              _ <-
                pure button # set UI.text player
                  # set UI.value player
                  # set (UI.attr "style") blockButtonStyleStr
              checkGame player

  _ <- getBody window #+ [gameBody]

  void . sequence $ clickOnButton <$> buttonLs -- main loop.

main :: IO ()
main = startGUI defaultConfig setup
