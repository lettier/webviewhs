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
main =
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle      = "webviewhs - How do I inject some custom CSS into the Window?"
        -- This could be a localhost URL to your single-page application (SPA).
      , WHS.windowParamsUri        = "https://lettier.github.com"
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
    -- This is the callback JavaScript can execute.
    (\ _window text -> print text)
    -- This function runs before the loop.
    (WHS.WithWindowLoopSetUp    (\ _window -> print ("Setting up." :: Data.Text.Text)))
    -- This function runs after the loop.
    (WHS.WithWindowLoopTearDown (\ _window -> print ("Tearing down." :: Data.Text.Text)))
    -- This function runs every window loop.
    -- Return True to continue the loop or False to exit the loop.
    $ \ window -> do
      let color = "#0000ff"
      -- injectCss' returns either True on success or False on failure.
      -- If you rather not use Clay, you can use injectCss'.
      success <-
        WHS.injectCss'
          window
          $ Data.Text.concat
              [ "div { color: "
              , color
              , "; }"
              ]
      return success
