{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
#-}

import Data.Text
import qualified Data.Text.Lazy as DTL
import Data.Text.Format.Heavy
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main =
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle      = "webviewhs - How do I log some debug information?"
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
    $ \ _window -> do
      -- webviewhs provides log and log'.
      -- log uses text-format-heavy which provides a "full-featured string
      -- formatting function, similar to Python's string.format."
      -- For more information, see https://github.com/portnov/text-format-heavy/wiki/.
      -- According to webview, logging will print to
      -- "stderr, MacOS Console or [Windows] DebugView."
      let string = "world" :: Text
      WHS.log "Hello {string}!" [("string" :: DTL.Text, Variable string)]
      WHS.log "{} {} {}" (1 :: Int, 2 :: Int, 3 :: Int)
      -- log' takes a simple Text string.
      WHS.log' "Hello world!"
      return True
