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
  ( WindowParams(..)
  , WindowBackgroundColor(..)
  , WindowAlertDialogType(..)
  , Window
  , WithWindowLoopSetUp(..)
  , WithWindowLoopTearDown(..)
  , createWindowAndBlock
  , createWindow
  , setWindowTitle
  , setWindowFullscreen
  , setWindowBackgroundColor
  , withWindowLoop
  , iterateWindowLoop
  , runJavaScript'
#ifndef LIGHT
  , runJavaScript
  , injectCss
  , Graphics.UI.Webviewhs.log
#endif
  , injectCss'
  , openWindowAlertDialog
  , withWindowOpenDialog
  , withWindowSaveDialog
  , dispatchToMain
  , Graphics.UI.Webviewhs.log'
  , terminateWindowLoop
  , destroyWindow
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

-- | Specifies the RGBA for the window background color.
data WindowBackgroundColor =
  WindowBackgroundColor
    { windowBackgroundColorRed   :: Word8
    , windowBackgroundColorGreen :: Word8
    , windowBackgroundColorBlue  :: Word8
    , windowBackgroundColorAlpha :: Word8
    }

-- | Specifies the window alert dialog type.
data  WindowAlertDialogType =
      WindowAlertDialogTypeInfo
    | WindowAlertDialogTypeWarning
    | WindowAlertDialogTypeError

newtype WithWindowLoopSetUp    a = WithWindowLoopSetUp    (Window a -> IO ())
newtype WithWindowLoopTearDown a = WithWindowLoopTearDown (Window a -> IO ())

windowDialogTypeAlert :: CInt
windowDialogTypeAlert = 2

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
      ->  FunPtr (Window a -> CString -> IO ())
      ->  IO (Window a)

foreign import ccall "webview-ffi.h c_set_window_title"
  c_set_window_title
      ::  Window a
      ->  CString
      ->  IO ()

foreign import ccall "webview-ffi.h c_set_window_fullscreen"
  c_set_window_fullscreen
      ::  Window a
      ->  CInt
      ->  IO ()

foreign import ccall "webview-ffi.h c_set_window_background_color"
  c_set_window_background_color
      ::  Window a
      ->  CUChar
      ->  CUChar
      ->  CUChar
      ->  CUChar
      ->  IO ()

foreign import ccall "webview-ffi.h c_iterate_window"
  c_iterate_window
      ::  Window a
      ->  CInt
      ->  IO CInt

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

foreign import ccall "webview-ffi.h c_open_window_dialog"
  c_open_window_dialog
      ::  Window a
      ->  CInt    -- Type
      ->  CInt    -- Flags
      ->  CString -- Primary Text / Title
      ->  CString -- Secondary Text
      ->  CString -- Buffer to store result
      ->  CUInt   -- Result buffer size
      ->  IO ()

foreign import ccall safe "webview-ffi.h c_dispatch_to_main"
  c_dispatch_to_main
      ::  Window a
      ->  FunPtr (Window a -> Ptr () -> IO () )
      ->  Ptr ()
      ->  IO ()

foreign import ccall "webview-ffi.h c_log"
  c_log
      ::  CString
      ->  IO ()

foreign import ccall "webview-ffi.h c_terminate_window_loop"
  c_terminate_window_loop
      ::  Window a
      ->  IO ()

foreign import ccall "webview-ffi.h c_destroy_window"
  c_destroy_window
      ::  Window a
      ->  IO ()

foreign import ccall "wrapper"
  makeDispatchCallback
      ::  (Window a -> Ptr () -> IO ())
      ->  IO (FunPtr (Window a -> Ptr () -> IO ()))

foreign import ccall "wrapper"
  makeCallback
      ::  (Window a -> CString -> IO ())
      ->  IO (FunPtr (Window a -> CString -> IO ()))

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

-- | Creates a window giving you the chance to changes its properties, run its loop, etc.
-- Returns 'Left' on failure and 'Right' 'Window' on success.
createWindow
  ::  WindowParams
  ->  (Window a -> Text -> IO ()) -- ^ A callback that JavaScript can use to
                                  -- communicate with the Haskell side.
  ->  IO (Either Text (Window a))
createWindow
  windowParams
  callback
  = do
  CWindowParams
    { cWindowParamsTitle
    , cWindowParamsUri
    , cWindowParamsWidth
    , cWindowParamsHeight
    , cWindowParamsResizable
    , cWindowParamsDebuggable
    } <- windowParamsToC windowParams
  let callback' window cString = do
        string <- peekCString cString
        callback window (Data.Text.pack string)
  funPtr <- makeCallback callback'
  result <-
    c_create_window
      cWindowParamsTitle
      cWindowParamsUri
      cWindowParamsWidth
      cWindowParamsHeight
      cWindowParamsResizable
      cWindowParamsDebuggable
      funPtr
  if result == nullPtr
    then do
      free cWindowParamsTitle
      free cWindowParamsUri
      freeHaskellFunPtr funPtr
      return $ Left "[WEBVIEWHS:ERROR] Could not create window."
    else do
      free cWindowParamsTitle
      free cWindowParamsUri
      return $ Right result

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

-- | Sets the window's fullscreen state.
-- Pass 'True' to put the window into fullscreen mode.
-- Pass 'False' to take the window out of fullscreen mode.
setWindowFullscreen
  ::  Window a
  ->  Bool
  ->  IO ()
setWindowFullscreen
  window
  fullscreen
  = do
  let fullscreen' = if fullscreen then 1 else 0
  c_set_window_fullscreen window fullscreen'

-- | If the loaded web page does not specify a background color,
-- this sets the window's background color.
setWindowBackgroundColor
  ::  Window a
  ->  WindowBackgroundColor
  ->  IO ()
setWindowBackgroundColor
  window
  WindowBackgroundColor
    { windowBackgroundColorRed
    , windowBackgroundColorGreen
    , windowBackgroundColorBlue
    , windowBackgroundColorAlpha
    }
  = do
  let red'   = fromIntegral windowBackgroundColorRed   :: CUChar
  let green' = fromIntegral windowBackgroundColorGreen :: CUChar
  let blue'  = fromIntegral windowBackgroundColorBlue  :: CUChar
  let alpha' = fromIntegral windowBackgroundColorAlpha :: CUChar
  c_set_window_background_color window red' green' blue' alpha'

-- | Iterates the window loop.
-- If 'True', runs the window loop continuously—blocking until the window exits.
-- If 'False', runs one iteration of the window loop
-- and releases control back to the caller.
iterateWindowLoop
  ::  Window a
  ->  Bool -- ^ Pass 'True' to iterate until the window exits.
           -- Pass 'False' to run one iteration.
  ->  IO Bool
iterateWindowLoop
  window
  block
  = do
  result <-
    c_iterate_window
      window
      (if block then 1 else 0)
  return (result == 0)

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

-- | Opens a window alert dialog.
openWindowAlertDialog
  ::  Window a
  ->  WindowAlertDialogType
  ->  Text -- ^ This is the primary message.
  ->  Text -- ^ This is the secondary message.
  ->  IO ()
openWindowAlertDialog
  window
  windowAlertDialogType
  primaryMessage
  secondaryMessage
  = do
  primaryMessage'   <- newCString $ Data.Text.unpack primaryMessage
  secondaryMessage' <- newCString $ Data.Text.unpack secondaryMessage
  result            <- newCString ""
  c_open_window_dialog
    window
    windowDialogTypeAlert
    (dialogType windowAlertDialogType)
    primaryMessage'
    secondaryMessage'
    result
    0
  free primaryMessage'
  free secondaryMessage'
  free result
  where
    dialogType :: WindowAlertDialogType -> CInt
    dialogType WindowAlertDialogTypeInfo    = 2
    dialogType WindowAlertDialogTypeWarning = 4
    dialogType WindowAlertDialogTypeError   = 6

-- | Opens a native file chooser dialog.
-- Accepts a callback that receives the selection.
withWindowOpenDialog
  ::  Window a
  ->  Text -- ^ The open dialog window title.
  ->  Bool -- ^ Pass 'True' to disable selecting files.
           -- Pass 'False' to allow selecting both files and directories.
  ->  (Text -> IO ()) -- ^ A callback that accepts the result of the dialog.
  ->  IO ()
withWindowOpenDialog
  window
  title
  =
  withWindowFileDialog
    window
    title
    True

-- | Opens a native file saving dialog.
-- Accepts a callback that receives the selection.
-- Does not actually save the file.
-- Save the file inside the provided callback.
withWindowSaveDialog
  ::  Window a
  ->  Text -- ^ The save dialog window title.
  ->  (Text -> IO ()) -- ^ A callback that accepts the result of the dialog.
  ->  IO ()
withWindowSaveDialog
  window
  title
  =
  withWindowFileDialog
    window
    title
    False
    False

withWindowFileDialog
  ::  Window a
  ->  Text
  ->  Bool
  ->  Bool
  ->  (Text -> IO ())
  ->  IO ()
withWindowFileDialog
  window
  title
  open
  disableOpeningFiles
  callback
  = do
  title'          <- newCString $ Data.Text.unpack title
  message'        <- newCString ""
  let bufferSize  = 1024
  let bufferSize' = fromIntegral bufferSize :: CUInt
  result          <- callocBytes bufferSize
  c_open_window_dialog
    window
    (if open then 0 else 1)
    (if disableOpeningFiles then 1 else 0)
    title'
    message'
    result
    bufferSize'
  free title'
  free message'
  result' <- peekCString result
  callback $ Data.Text.pack result'
  free result

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

-- | Logs the given input to stderr, macOS console,
-- or Windows DebugView depending on the build platform.
log'
  ::  Text
  ->  IO ()
log'
  entry
  = do
  entry' <- newCString $ Data.Text.unpack entry
  c_log entry'
  free entry'
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
  ->  (Window a -> Text -> IO ()) -- ^ A callback function that can be invoked from the JavaScript side.
                                  -- The callback must accept a 'Window' and the JavaScript sent 'Text'.
                                  -- The JavaScript sent 'Text' could be unstructured or structured like JSON.
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
  ->  (Window a -> IO Bool)       -- ^ A function that is called each iteration.
                                  -- Return 'True' to continue or 'False' to stop iterating.
  ->  IO ()
withWindowLoop
  windowParams
  callback
  (WithWindowLoopSetUp setUp)
  (WithWindowLoopTearDown tearDown)
  iteration
  = do
  eitherWindow   <- createWindow windowParams callback
  case eitherWindow of
    Left  e      -> putStrLn $ Data.Text.unpack e
    Right window -> do
      setUp               window
      loop                window iteration
      tearDown            window
      terminateWindowLoop window
      destroyWindow       window
  where
    loop :: Window a -> (Window a -> IO Bool) -> IO ()
    loop window iteration' = do
      continue        <- iteration'        window
      shouldContinue  <- iterateWindowLoop window False
      when (continue && shouldContinue) $
        loop window iteration'

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

-- | Logs the given formatted input to stderr, macOS console,
-- or Windows DebugView depending on the build platform.
-- Uses [Data.Text.Format.Heavy](https://hackage.haskell.org/package/text-format-heavy).
-- Note, this function is not available when using the `light` cabal build flag.
log
  ::  VarContainer vars
  =>  Format
  ->  vars
  ->  IO ()
log
  fmt
  vars
  = do
  let entry = format fmt vars
  log' $ DTL.toStrict entry
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
