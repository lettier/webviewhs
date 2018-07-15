{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
#-}

import Data.Text
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  eitherWindow <-
    WHS.createWindow
      WHS.WindowParams
        { WHS.windowParamsTitle      = "webviewhs - How do I open a save dialog?"
          -- This could be a localhost URL to your single-page application (SPA).
        , WHS.windowParamsUri        = "https://lettier.github.com"
        , WHS.windowParamsWidth      = 800
        , WHS.windowParamsHeight     = 600
        , WHS.windowParamsResizable  = True
        , WHS.windowParamsDebuggable = True
        }
      -- This is the callback JavaScript can execute.
      (\ _window text -> print text)
  case eitherWindow of
    Left  e      -> print e
    Right window -> do
      let callback saveDialogResult = do
            putStrLn $
                 "This is the save dialog result: "
              ++ Data.Text.unpack saveDialogResult
            putStrLn "Done."
      WHS.withWindowSaveDialog
        window
        "This is the window save dialog title."
        callback
