{-
  webviewhs
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
      ForeignFunctionInterface
    , NamedFieldPuns
    , OverloadedStrings
    , CPP
#-}

{-|
  Module      : Graphics.UI.Webviewhs
  Description : Create native dialogs and windows that run web pages.
  Copyright   : (C) 2018 David Lettier
  License     : BSD3
  Maintainer  : David Lettier

  ![webviewhs logo](https://i.imgur.com/2yAJALE.png)

  webviewhs is a Haskell binding to the [webview](https://github.com/zserge/webview) library created by
  [Serge Zaitsev](https://github.com/zserge).

  According to webview:

  > [webview is] a tiny cross-platform webview library for C/C++/Golang to build modern cross-platform GUIs.
  > It uses Cocoa/WebKit on macOS, gtk-webkit2 on Linux and MSHTML (IE10/11) on Windows.

  For more information, see the webview
  [README](https://github.com/zserge/webview/blob/d007fc53b107f6043c2a6a3372548dbf59dfe876/README.md).

  Be sure to explore the provided [examples](https://github.com/lettier/webviewhs/tree/master/examples#readme).

  To exclude clay, jmacro, and text-format-heavy, use the cabal build flag `light`.
  Be sure to explore the provided [light examples](https://github.com/lettier/webviewhs/tree/master/examples-light#readme)
  for more information.
-}

module Graphics.UI.Webviewhs
  ( RequestResponseType(..)
  , WindowParams(..)
  , Window
  , WithWindowLoopSetUp(..)
  , WithWindowLoopTearDown(..)
  , createWindowAndBlock
  , createWindow
  , setWindowTitle
  , withWindowLoop
  , iterateWindowLoop
  , runJavaScript'
#ifndef LIGHT
  , runJavaScript
  , injectCss
#endif
  , injectCss'
  , dispatchToMain
  , terminateWindowLoop
  , destroyWindow
  , bindCallback
  , respondRequest
  )
where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Concurrent.MVar
import Data.Word
import Data.Text

#ifndef LIGHT
import qualified Data.Text.Lazy as DTL
import Language.Javascript.JMacro
import Data.Text.Format.Heavy
import Clay (Css, render)
#endif

-- | Pointer to a webview struct.
type Window = Ptr

-- | Specifies the window creation parameters.
data WindowParams =
  WindowParams
    { windowParamsTitle      :: Text
    -- | This can be file:\/\/, http:\/\/, https:\/\/, data:text/html, etc.
    , windowParamsUri        :: Text
    , windowParamsWidth      :: Int
    , windowParamsHeight     :: Int
    , windowParamsResizable  :: Bool
    -- | This enables the right click context menu with reload and
    -- Web Inspector options for GTK WebKit and Cocoa WebKit.
    -- When using WebKit, it also enables JavaScript `console.log`
    -- and similar methods the ability to write to stdout.
    -- It has no affect on Windows.
    -- According to webview, "on Windows there is no easy to way to enable
    -- debugging, but you may include Firebug in your HTML code."
    , windowParamsDebuggable :: Bool
    }

data CWindowParams =
  CWindowParams
    { cWindowParamsTitle      :: CString
    , cWindowParamsUri        :: CString
    , cWindowParamsWidth      :: CInt
    , cWindowParamsHeight     :: CInt
    , cWindowParamsResizable  :: CInt
    , cWindowParamsDebuggable :: CInt
    }

data RequestResponseType
  = RequestReject
  | RequestResolve

newtype WithWindowLoopSetUp    a = WithWindowLoopSetUp    (Window a -> IO ())
newtype WithWindowLoopTearDown a = WithWindowLoopTearDown (Window a -> IO ())

foreign import ccall "webview-ffi.h c_create_window_and_block"
    c_create_window_and_block
      ::  CString
      ->  CString
      ->  CInt
      ->  CInt
      ->  CInt
      ->  CInt
      ->  IO ()

foreign import ccall "webview-ffi.h c_create_window"
  c_create_window
      ::  CString
      ->  CString
      ->  CInt
      ->  CInt
      ->  CInt
      ->  CInt
      ->  IO (Window a)

foreign import ccall "webview-ffi.h c_set_window_title"
  c_set_window_title
      ::  Window a
      ->  CString
      ->  IO ()

foreign import ccall "webview-ffi.h c_iterate_window"
  c_iterate_window
      ::  Window a
      ->  IO ()

foreign import ccall "webview-ffi.h c_run_javascript"
  c_run_javascript
      ::  Window a
      ->  CString
      ->  IO CInt

foreign import ccall "webview-ffi.h c_inject_css"
  c_inject_css
      ::  Window a
      ->  CString
      ->  IO CInt

foreign import ccall safe "webview-ffi.h c_dispatch_to_main"
  c_dispatch_to_main
      ::  Window a
      ->  FunPtr (Window a -> Ptr () -> IO () )
      ->  Ptr ()
      ->  IO ()

foreign import ccall "webview-ffi.h c_terminate_window_loop"
  c_terminate_window_loop
      ::  Window a
      ->  IO ()

foreign import ccall "webview-ffi.h c_destroy_window"
  c_destroy_window
      ::  Window a
      ->  IO ()

foreign import ccall "webview-ffi.h c_bind_callback"
  c_bind_callback
      ::  Window a
      ->  CString
      ->  FunPtr (CString -> CString -> Ptr b -> IO ())
      ->  Ptr b
      ->  IO ()

foreign import ccall "webview-ffi.c c_return_response"
  c_return_response
      ::  Window a
      ->  CString
      ->  CInt
      ->  CString
      ->  IO ()

foreign import ccall "wrapper"
  makeDispatchCallback
      ::  (Window a -> Ptr () -> IO ())
      ->  IO (FunPtr (Window a -> Ptr () -> IO ()))

foreign import ccall "wrapper"
  makeCallback
      ::  (CString -> CString -> Ptr b -> IO ())
      ->  IO (FunPtr (CString -> CString-> Ptr b -> IO ()))

-- | Creates a window and runs the main loop unless the window is destroyed.
-- Useful for loading a web page and not having to manage the loop.
createWindowAndBlock
  ::  WindowParams
  ->  IO ()
createWindowAndBlock
  windowParams
  = do
  CWindowParams
    { cWindowParamsTitle
    , cWindowParamsUri
    , cWindowParamsWidth
    , cWindowParamsHeight
    , cWindowParamsResizable
    , cWindowParamsDebuggable
    } <- windowParamsToC windowParams
  c_create_window_and_block
    cWindowParamsTitle
    cWindowParamsUri
    cWindowParamsWidth
    cWindowParamsHeight
    cWindowParamsResizable
    cWindowParamsDebuggable
  free cWindowParamsTitle
  free cWindowParamsUri

-- | Creates a window giving you the chance to changes its properties.
-- Returns 'Left' on failure and 'Right' 'Window' on success.
createWindow
  ::  WindowParams
  ->  IO (Either Text (Window a))
createWindow
  windowParams
  = do
  CWindowParams
    { cWindowParamsTitle
    , cWindowParamsUri
    , cWindowParamsWidth
    , cWindowParamsHeight
    , cWindowParamsResizable
    , cWindowParamsDebuggable
    } <- windowParamsToC windowParams
  result <-
    c_create_window
      cWindowParamsTitle
      cWindowParamsUri
      cWindowParamsWidth
      cWindowParamsHeight
      cWindowParamsResizable
      cWindowParamsDebuggable
  if result == nullPtr
    then do
      free cWindowParamsTitle
      free cWindowParamsUri
      return $ Left "[WEBVIEWHS:ERROR] Could not create window."
    else do
      free cWindowParamsTitle
      free cWindowParamsUri
      return $ Right result

-- | Expose a callback as global JavaScript function.
bindCallback
  ::  Window a
  ->  Text -- ^ Callback name
  ->  (Window a -> Text -> Text -> b -> IO ()) 
      -- ^ A callback that JavaScript can use to
      -- communicate with the Haskell side.
  ->  b -- ^ User defined data
  ->  IO ()
bindCallback
  window
  callbackName
  callback
  userData
  = do
  callbackName' <- newCString $ Data.Text.unpack callbackName
  let callback' cSeq cReq _ = do
        seq <- peekCString cSeq
        req <- peekCString cReq
        callback window (Data.Text.pack seq) (Data.Text.pack req) userData
  funPtr <- makeCallback callback'
  c_bind_callback window callbackName' funPtr nullPtr

-- | Send a value to answer request from webview
respondRequest
  :: Window a
  -> Text -- ^ request id
  -> RequestResponseType
  -> Text -- ^ json encoded result
  -> IO ()
respondRequest window reqId responseType result = do
  reqId' <- newCString $ Data.Text.unpack reqId
  status' <- case responseType of
    RequestReject  -> pure 1
    RequestResolve -> pure 0
  result' <- newCString $ Data.Text.unpack result
  c_return_response window reqId' status' result'

-- | Changes the window title.
setWindowTitle
  ::  Window a
  ->  Text
  ->  IO ()
setWindowTitle
  window
  newTitle
  = do
  newTitle' <- newCString $ Data.Text.unpack newTitle
  c_set_window_title window newTitle'
  free newTitle'

-- | Runs the window loop continuously — blocking until the window exits.
iterateWindowLoop
  ::  Window a
  ->  IO ()
iterateWindowLoop
  window
  = do
  c_iterate_window window

-- | Runs the given JavaScript inside the window.
-- The given JavaScript is not checked for validity.
runJavaScript'
  ::  Window a
  ->  Text
  ->  IO Bool -- ^ Returns 'True' on success and 'False' on failure.
runJavaScript'
  window
  javaScript
  = do
  javaScript' <- newCString $ Data.Text.unpack javaScript
  result      <- c_run_javascript window javaScript'
  free javaScript'
  return (result /= -1)


-- | Injects CSS into the window.
-- The given CSS is not checked for validity.
injectCss'
  ::  Window a
  ->  Text
  ->  IO Bool -- ^ Returns 'True' on success and 'False' on failure.
injectCss'
  window
  css
  = do
  css'   <- newCString $ Data.Text.unpack css
  result <- c_inject_css window css'
  free css'
  return (result /= -1)

-- | Runs the given function in the main window UI thread.
-- Use this function whenever you wish to interact with the
-- window but you're not running in the main window UI thread.
dispatchToMain
  ::  Window a
  ->  (Window a -> IO ())
  ->  IO ()
dispatchToMain
  window
  callback
  = do
  mvar                    <- newEmptyMVar
  let callback' window' _ = do
        callback window'
        putMVar mvar (1 :: Int)
  funPtr                  <- makeDispatchCallback callback'
  let nullPtr'            = nullPtr
  c_dispatch_to_main window funPtr nullPtr'
  _ <- takeMVar mvar
  freeHaskellFunPtr funPtr
  return ()

-- | Terminates the window's loop.
terminateWindowLoop
  ::  Window a
  ->  IO ()
terminateWindowLoop = c_terminate_window_loop

-- | Destroys the window.
destroyWindow
  ::  Window a
  ->  IO ()
destroyWindow = c_destroy_window

-- | Manages the window and main loop for you.
-- It accepts a JavaScript callback, setup, teardown, and iteration function.
withWindowLoop
  ::  WindowParams
  ->  WithWindowLoopSetUp    a    -- ^ A function that is called before iterating.
                                  -- It must accept a 'Window' and return 'IO' ().
                                  -- Use it to set up before entering the main loop.
                                  -- You can pass 'void' '.' 'return' '.' 'const' if you
                                  -- don't have a setup function.
  ->  WithWindowLoopTearDown a    -- ^ A function that is called after iterating.
                                  -- It must accept a 'Window' and return 'IO' ().
                                  -- Use it to tear down after leaving the main loop.
                                  -- You can pass 'void' '.' 'return' '.' 'const' if you
                                  -- don't have a teardown function.
                                  -- Note, do not terminate the window loop and/or
                                  -- destroy the window as this is done for you.
  ->  IO ()
withWindowLoop
  windowParams
  (WithWindowLoopSetUp setUp)
  (WithWindowLoopTearDown tearDown)
  = do
  eitherWindow   <- createWindow windowParams
  case eitherWindow of
    Left  e      -> putStrLn $ Data.Text.unpack e
    Right window -> do
      setUp               window
      iterateWindowLoop   window
      tearDown            window
      terminateWindowLoop window
      destroyWindow       window

#ifndef LIGHT
-- | Runs the given JavaScript inside the window.
-- Uses [Language.Javascript.JMacro](https://hackage.haskell.org/package/jmacro).
-- Note, this function is not available when using the `light` cabal build flag.
runJavaScript
  ::  (JsToDoc js, JMacro js)
  =>  Window a
  ->  js
  ->  IO Bool -- ^ Returns 'True' on success and 'False' on failure.
runJavaScript
  window
  javaScript
  = do
  let javaScript' = Data.Text.pack $ show $ renderJs javaScript
  runJavaScript' window javaScript'

-- | Injects CSS into the window.
-- Uses [Clay](https://hackage.haskell.org/package/clay).
-- Note, this function is not available when using the `light` cabal build flag.
injectCss
  ::  Window a
  ->  Clay.Css
  ->  IO Bool -- ^ Returns 'True' on success and 'False' on failure.
injectCss
  window
  css
  =
  injectCss'
    window $
      DTL.toStrict $
        Clay.render css
#endif

windowParamsToC
  ::  WindowParams
  ->  IO CWindowParams
windowParamsToC
  WindowParams
    { windowParamsTitle
    , windowParamsUri
    , windowParamsWidth
    , windowParamsHeight
    , windowParamsResizable
    , windowParamsDebuggable
    }
  = do
  title          <- newCString $ Data.Text.unpack windowParamsTitle
  uri            <- newCString $ Data.Text.unpack windowParamsUri
  let width      = fromIntegral windowParamsWidth  :: CInt
  let height     = fromIntegral windowParamsHeight :: CInt
  let resizable  = if windowParamsResizable then 1 else 0
  let debuggable = if windowParamsDebuggable then 1 else 0
  return
    CWindowParams
      { cWindowParamsTitle      = title
      , cWindowParamsUri        = uri
      , cWindowParamsWidth      = width
      , cWindowParamsHeight     = height
      , cWindowParamsResizable  = resizable
      , cWindowParamsDebuggable = debuggable
      }
