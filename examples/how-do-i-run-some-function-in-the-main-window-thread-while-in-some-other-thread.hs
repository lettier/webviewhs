{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
#-}

import System.Random
import Data.Maybe
import Data.Text
import Control.Monad
import Control.Concurrent
import Language.Javascript.JMacro
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  counter  <- newMVar (0 :: Int)
  continue <- newMVar True
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle      = "webviewhs - How do I run some function in the main window thread while in some other thread?"
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
      counter' <- takeMVar counter
      -- Every so often, change the web page background color to a random color.
      if counter' >= 100000
        then do
          putMVar counter 0
          -- Instead of changing the background color in the main thread,
          -- we'll change it from another thread by dispatching the
          -- background-changing function to the window's main thread.
          _ <- forkIO $
            WHS.dispatchToMain
              window $
              \ window' -> do
                red   <- randomRIO (0 :: Int, 255)
                green <- randomRIO (0 :: Int, 255)
                blue  <- randomRIO (0 :: Int, 255)
                success <-
                  WHS.runJavaScript
                    window'
                    -- runJavaScript uses JMacro which is a
                    -- "simple DSL for lightweight (untyped) programmatic generation of Javascript."
                    [jmacro|
                      fun setBackgroundColor r g b {
                        var color = "rgba(" + r + ", " + g + ", " + b + ", 1)";
                        window.external.invoke("Changing the background color to " + color);
                        document.body.style.backgroundColor = color;
                      };
                      setTimeout(
                        \ -> setBackgroundColor `(red)` `(green)` `(blue)`,
                        1000
                      );
                    |]
                void $ tryPutMVar continue success
          fromMaybe True <$> tryTakeMVar continue
        else do
          putMVar counter $ counter' + 1
          return True
