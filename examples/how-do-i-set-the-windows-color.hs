{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
#-}

import Control.Concurrent
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  eitherWindow <-
    WHS.createWindow
      WHS.WindowParams
        { WHS.windowParamsTitle      = "webviewhs - How do I set the window's color?"
          -- This could be a localhost URL to your single-page application (SPA).
          -- If a web page specifies a background color, setBackgroundColor will be
          -- overridden.
        , WHS.windowParamsUri        = ""
        , WHS.windowParamsWidth      = 800
        , WHS.windowParamsHeight     = 600
        , WHS.windowParamsResizable  = True
        , WHS.windowParamsDebuggable = True
        }
      -- This is the callback JavaScript can execute.
      (\ _window text -> print text)
  case eitherWindow of
    Left  e       -> print e
    Right window -> do
      _ <- forkIO $ do
        -- In two seconds, set the window's background color to green.
        threadDelay 2000000
        WHS.dispatchToMain window $ \ window' ->
          WHS.setWindowBackgroundColor
            window'
            WHS.WindowBackgroundColor
              { WHS.windowBackgroundColorRed   = 0
              , WHS.windowBackgroundColorGreen = 255
              , WHS.windowBackgroundColorBlue  = 0
              , WHS.windowBackgroundColorAlpha = 255
              }
      -- Run the window loop and block.
      continue <- WHS.iterateWindowLoop window True
      putStrLn $ "Could continue? " ++ show continue
