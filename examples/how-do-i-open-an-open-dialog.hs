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
        { WHS.windowParamsTitle      = "webviewhs - How do I open an open dialog?"
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
      let disableOpeningFiles = False
      let callback openDialogResult = do
            putStrLn $
                 "This is the open dialog result: "
              ++ Data.Text.unpack openDialogResult
            putStrLn "Done."
      WHS.withWindowOpenDialog
        window
        "This is the window open dialog title."
        disableOpeningFiles
        callback
      WHS.withWindowOpenDialog
        window
        "This is the window open dialog title."
        (not disableOpeningFiles)
        callback
