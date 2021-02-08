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
import Data.Text
import Data.Text.Encoding
import Data.ByteString.Lazy
import Data.Aeson
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
    -- This function runs before the loop.
    (WHS.WithWindowLoopSetUp setUp)
    -- This function runs after the loop.
    (WHS.WithWindowLoopTearDown tearDown)
  where
    callback _window _ dataFromJavaScript () = do
      print dataFromJavaScript
      print (decode (fromStrict $ encodeUtf8 dataFromJavaScript) :: Maybe JsonMessage)
    -- This function runs every window loop.
    foo _window _ dataFromJavaScript () = do
      print dataFromJavaScript
      let message' = "Hello from JavaScript." :: Text
      -- runJavaScript' returns either True on success or False on failure.
      WHS.runJavaScript'
        _window
        $ Data.Text.concat
            [ "window.callback(JSON.stringify({ message: \""
            , message'
            , "\" }));"
            ]
      pure ()

    setUp _window = do
      print ("Setting up." :: Data.Text.Text)
      -- This is a callback JavaScript can execute.
      -- Inside JavaScript, you call "window.callback".
      WHS.bindCallback _window "callback" callback ()
      WHS.bindCallback _window "foo" foo ()
      WHS.runJavaScript' _window "setTimeout(function f(){ console.log('called f'); window.foo(''); setTimeout(f, 0); }, 1000)"
      pure ()
    tearDown _window = print ("Tearing down." :: Data.Text.Text)