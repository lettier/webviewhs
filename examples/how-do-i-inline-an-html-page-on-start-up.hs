{-
  webviewhs
  (C) 2019 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
#-}

import Control.Monad
import Data.Text
import Language.Javascript.JMacro
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main =
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle      = "webviewhs - How do I inline an HTML page on start up?"
        -- This can be "http://", "https://", "file://", or "data:text/html,".
      , WHS.windowParamsUri        =
        Data.Text.unlines
          [ "data:text/html,"
          , "<html>"
          , " <head>"
          , "  <title>webviewhs</title>"
          , "  <style>"
          , "   button   { background-color: #1ae; color: #111; }"
          , "   .rotated { transform: rotate(30deg); }"
          , "  </style>"
          , " </head>"
          , " <body>"
          , "  <button id='button'>Button</button>"
          , "  <script>"
          , "   var button = document.getElementById('button');"
          , "   setTimeout("
          , "    function() {"
          , "     button.style.backgroundColor = '#ea1';"
          , "    },"
          , "    1000"
          , "   );"
          , "   /* When the button is clicked, invoke the Haskell callback. */"
          , "   button.addEventListener('click', function() {"
          , "    /* Send the Haskell callback the message 'clicked'. */"
          , "    window.external.invoke('clicked');"
          , "   });"
          , "  </script>"
          , " </body>"
          , "</html>"
          ]
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
    -- This is the callback JavaScript can execute.
    (\ window text ->
      -- If the message is "clicked"...
      when (text == "clicked")
        $ void
          $ WHS.runJavaScript
            window
            [jmacro|
              // ...toggle the rotation of the button.
              var button = document.getElementById("button");
              button.classList.toggle("rotated");
            |]
    )
    -- This function runs before the loop.
    (WHS.WithWindowLoopSetUp    (\ _window -> print ("Setting up." :: Data.Text.Text)))
    -- This function runs after the loop.
    (WHS.WithWindowLoopTearDown (\ _window -> print ("Tearing down." :: Data.Text.Text)))
    -- This function runs every window loop.
    -- Return True to continue the loop or False to exit the loop.
    (return . const True)
