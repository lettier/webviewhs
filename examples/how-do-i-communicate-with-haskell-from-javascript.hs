{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , QuasiQuotes
#-}

import GHC.Generics
import Control.Monad
import Data.Text
import Data.Text.Encoding
import Data.ByteString.Lazy
import Data.Aeson
import Language.Javascript.JMacro
import qualified Graphics.UI.Webviewhs as WHS

data JsonMessage =
  JsonMessage
    { _message :: Text
    } deriving (Generic, Show)

instance FromJSON JsonMessage

main :: IO ()
main =
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle      = "webviewhs - How do I communicate with Haskell from JavaScript?"
        -- This could be a localhost URL to your single-page application (SPA).
      , WHS.windowParamsUri        = "https://lettier.github.com"
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
    -- This is the callback JavaScript can execute.
    -- Inside JavaScript, you call "window.external.invoke".
    (\ _window stringFromJavaScript -> do
      print stringFromJavaScript
      print (decode (fromStrict $ encodeUtf8 stringFromJavaScript) :: Maybe JsonMessage)
    )
    -- This function runs before the loop.
    (WHS.WithWindowLoopSetUp    (\ _window -> print ("Setting up." :: Data.Text.Text)))
    -- This function runs after the loop.
    (WHS.WithWindowLoopTearDown (void . return . const))
    -- This function runs every window loop.
    -- Return True to continue the loop or False to exit the loop.
    $ \ window -> do
      let message' = "Hello from JavaScript." :: Text
      -- runJavaScript returns either True on success or False on failure.
      success <-
        WHS.runJavaScript
          window
          [jmacro|
            window.external.invoke(JSON.stringify({ message: `(message')` }));
          |]
      -- If you rather not use JMacro, you can use runJavaScript'.
      success' <-
        WHS.runJavaScript'
          window
          "window.external.invoke(\"This won't decode.\");"
      return $ success && success'
