{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
#-}

import Clay
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
    (\ _window text -> print text) $
    -- This function runs every window loop.
    -- Return True to continue the loop or False to exit the loop.
    \ window -> do
      -- injectCss returns either True on success or False on failure.
      -- injectCss uses Clay a "a CSS preprocessor like LESS and Sass,
      -- but implemented as an embedded domain specific language (EDSL) in Haskell."
      success <-
        WHS.injectCss
          window $
          -- This turns all <div> text blue.
          Clay.div Clay.?
            Clay.color "#0000ff"
      -- If you rather not use Clay, you can use injectCss'.
      success' <-
        WHS.injectCss'
          window
          "body { background-color: white !important; }"
      return $ success && success'
