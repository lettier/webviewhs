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
        { WHS.windowParamsTitle      = "webviewhs - How do I terminate the window loop?"
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
    Left  e       -> print e
    Right window -> do
      _ <- forkIO $ do
        -- In two seconds, terminate the window loop.
        threadDelay 2000000
        WHS.dispatchToMain window $ \ window' ->
          WHS.terminateWindowLoop window'
      -- Run the window loop and block.
      continue <- WHS.iterateWindowLoop window True
      putStrLn $ "Could continue? " ++ show continue
      WHS.destroyWindow window
