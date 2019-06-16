{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
#-}

import Data.Text
import Language.Javascript.JMacro
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main =
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle      = "webviewhs - How do I run some JavaScript inside the window?"
        -- This could be a localhost URL to your single-page application (SPA).
      , WHS.windowParamsUri        = "https://lettier.github.com"
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
    -- This is the callback JavaScript can execute.
    (\ _window stringFromJavaScript -> print stringFromJavaScript)
    -- This function runs before the loop.
    (WHS.WithWindowLoopSetUp    (\ _window -> print ("Setting up." :: Data.Text.Text)))
    -- This function runs after the loop.
    (WHS.WithWindowLoopTearDown (\ _window -> print ("Tearing down." :: Data.Text.Text)))
    -- This function runs every window loop.
    -- Return True to continue the loop or False to exit the loop.
    $ \ window -> do
      let string = "Hello from Haskell." :: Text
      -- runJavaScript returns either True on success or False on failure.
      WHS.runJavaScript
        window
        [jmacro|
          var divv              = document.createElement("div");
          divv.style.height     = "500px";
          divv.style.width      = "500px";
          divv.style.color      = "white";
          divv.style.background = "black";
          divv.style.zIndex     = "1000";
          divv.style.position   = "absolute";
          divv.style.top        = "0px";
          divv.style.left       = "0px";
          divv.innerHTML        = `(string)`;
          document.body.appendChild(divv);
          window.external.invoke("Hello from JavaScript.");
        |]
