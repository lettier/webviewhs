![webviewhs logo](https://i.imgur.com/yohrYgX.png)

# What is webviewhs?

webviewhs is a Haskell binding to the [webview](https://github.com/zserge/webview) library created by
[Serge Zaitsev](https://github.com/zserge).

According to webview:

> [webview is] a tiny cross-platform webview library for C/C++/Golang to build modern cross-platform GUIs.
> It uses Cocoa/WebKit on macOS, gtk-webkit2 on Linux and MSHTML (IE10/11) on Windows.

For more information, see the webview
[README](https://github.com/zserge/webview/blob/d007fc53b107f6043c2a6a3372548dbf59dfe876/README.md).

webviewhs allows you to create native desktop windows and dialogs—while at the
same time—rich web-based UI experiences all wrapped up in the powerful, type-safe embrace of Haskell.

Coupled with [PureScript](http://www.purescript.org/) for the front-end portion,
you now have an end-to-end purely functional programming language solution for creating desktop apps.

Be sure to explore the provided [examples](examples).

## How complete is the binding?

- [x] webview
- [x] webview_init
- [x] webview_loop
- [x] webview_eval
- [x] webview_inject_css
- [x] webview_set_title
- [x] webview_set_fullscreen
- [x] webview_set_color
- [x] webview_dialog
- [x] webview_dispatch
- [x] webview_terminate
- [x] webview_exit
- [x] webview_debug
- [x] webview_print_log

## How do I install webviewhs?

In your `my-project.cabal` file, list `webviewhs` under `build-depends:` like so:

```yaml
  build-depends:
      base >= 4.7 && < 5
    , webviewhs
```

If you're using stack >= 1.7.1, put the following in your `stack.yaml`:

```yaml
extra-deps:
  - github: lettier/webviewhs
    commit: # Insert commit SHA1 here.
```

For older stack versions, put the following:

```yaml
extra-deps:
  - git: https://github.com/lettier/webviewhs.git
    commit: # Insert commit SHA1 here.
```

And now the run the following.

```bash
stack install --only-dependencies
```

If you're using cabal, run the following:

```bash
git clone https://github.com/lettier/webviewhs.git
cd my-project
cabal sandbox init
cabal sandbox add-source ../webviewhs
cabal --require-sandbox install --only-dependencies
```

Depending on your cabal version, you may be able to specify the
[git repository and commit](https://github.com/haskell/cabal/issues/2189#issuecomment-405058663)
much like stack.

## How do I use webviewhs?

If you want to open up a native desktop window that loads a web page and manages itself,
do the following:

```haskell
{-# LANGUAGE
    OverloadedStrings
#-}

import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "Test"
      , WHS.windowParamsUri        = "https://lettier.github.io"
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
```

If you want more control over the native desktop window, you could do something like this:

```haskell
{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
#-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.BoundedChan as CCBC
import Data.Maybe
import Data.Text
import qualified Data.Text.Lazy as DTL
import Data.Text.Format.Heavy
import Language.Javascript.JMacro
import qualified Clay
import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  -- Create a channel to communicate between the main thread and another thread you'll create.
  -- This isn't necessary but it's a great way to communicate between threads.
  channel <- newBoundedChan 1

  -- withWindowLoop handles the creation, iteration, and deletion of the window.
  WHS.withWindowLoop

    -- Set the window creation params.
    WHS.WindowParams
      { WHS.windowParamsTitle      = "Test"
        -- This could be a localhost URL to your single-page application (SPA).
      , WHS.windowParamsUri        = "https://lettier.github.com"
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True -- Enables the Web Inspector if using WebKit.
      }

    -- webview allows you to specify a callback function that can be
    -- called from the JavaScript side.
    -- The callback receives a single string parameter.
    -- This could be unstructured text or unparsed JSON for example.
    -- You can just print what was received for now.
    (\ _window stringFromJavaScript -> print stringFromJavaScript)

    -- This function runs before the loop.
    (WHS.WithWindowLoopSetUp    (\ _window -> print "Setting up."))

    -- This function runs after the loop.
    (WHS.WithWindowLoopTearDown (\ _window -> print "Tearing down."))

    -- If you don't need to set up and/or tear down anything, you can do this.
    -- (WHS.WithWindowLoopSetUp    (void . return . const))
    -- (WHS.WithWindowLoopTearDown (void . return . const))

    -- This function is called continuously.
    -- Return True to continue the window loop or
    -- return False to exit the loop and destroy the window.
    $ \ window -> do

      -- webviewhs provides log and log'.
      -- log uses text-format-heavy which provides a
      -- "full-featured string formatting function, similar to Python's string.format."
      -- log' takes a simple Text string.
      -- According to webview, logging will print to
      -- "stderr, MacOS Console or [Windows] DebugView."
      let string = "world" :: Text
      WHS.log "Hello {string}!" [("string" :: DTL.Text, Variable string)]

      -- webview allows you to run JS inside the window.
      -- webviewhs comes with runJavaScript and runJavaScript'.
      -- runJavaScript uses JMacro which is a
      -- "simple DSL for lightweight (untyped) programmatic generation of Javascript."
      -- runJavaScript' takes a Text string which may or may not be valid JavaScript.
      let red = "red" :: Text
      _ <- WHS.runJavaScript
        window

        -- This changes the web page background color to red.
        -- Notice that you can use Haskell values inside the JavaScript and
        -- even use Haskell like syntax.
        [jmacro|
          fun setBackgroundColor color { document.body.style.backgroundColor = color; }
          setTimeout(
            \ -> setBackgroundColor `(red)`,
            5000
          );
        |]

      -- webview allows you to inject CSS into the window.
      -- webviewhs offers injectCss and injectCss'.
      -- injectCss uses Clay "a CSS preprocessor like LESS and Sass,
      -- but implemented as an embedded domain specific language (EDSL) in Haskell."
      -- injectCss' takes a Text string which may or may not be valid CSS.
      _ <- WHS.injectCss
        window

        -- This turns all <div> text blue.
        $ Clay.div Clay.?
          Clay.color "#0000ff"

      -- Inside the window loop, create a thread.
      _ <- forkIO $ do
        WHS.log' "Hello from inside a thread."

        -- When you're not in the main window UI thread, you'll need to call
        -- dispatchToMain if you want to interact with the window.
        -- dispatchToMain will run the given function in the main UI thread.
        -- Note that dispatchToMain runs the function asynchronously with no guarantee
        -- as to when it will run.
        WHS.dispatchToMain
          window
          $ \ window' -> do
            result <-
              WHS.runJavaScript
                window'

                -- This will randomly scroll the web page up and down.
                [jmacro|
                  if (Math.random() < 0.1) {
                    setTimeout(
                      function() {
                        window.scrollTo(0, Math.random() * window.innerHeight);
                      },
                      10000
                    );
                  }
                |]

            -- runJavaScript returns True if it was successful and
            -- False if something went wrong.
            -- Here is an attempt to write the result to the channel.
            void $ CCBC.tryWriteChan channel result

      -- Exit the loop if you read False from the channel.
      -- Note that tryReadChan does not block which is
      -- important when inside the window loop.
      fromMaybe True <$> tryReadChan channel

  -- At this point,
  -- the loop has been exited,
  -- the window has been destroyed,
  -- and the program will now exit.
```

For more ways to use webviewhs,
take a look at the [examples](examples) directory.

## What if I don't want clay, jmacro, and text-format-heavy?

webviewhs has a `light` build flag that removes the dependencies clay, jmacro, and text-format-heavy.
In some cases, using the `light` build flag can reduce the final binary size by 77%.

Note that the `light` build flag removes `runJavaScript`, `injectCss`, and `log` from the API.
You can still use `runJavaScript'`, `injectCss'`, and `log'`.

If you're using stack, you can supply the `light` flag in the `stack.yaml` file.

```yaml
flags:
  webviewhs:
    light: true
```

You can also supply the `light` flag on the command line like so.

```bash
stack build --flag webviewhs:light
```

If you're using cabal, you'll have to supply a constraint for all configure and install commands.

```bash
# For configure.
cabal configure  --constraint="webviewhs +light"

# For install.
cabal install -j --constraint="webviewhs +light"
```

There's currently no way to supply the constraint in the cabal file itself,
however,
there is an [open issue](https://github.com/haskell/cabal/issues/2821) about it.

For more information about using the light version of webviewhs,
take a look at the [examples-light](examples-light) directory.

## What is the license?

For the webviewhs license information, see [LICENSE](LICENSE).
For the webview license information, see [deps/webview/LICENSE](deps/webview/LICENSE).

## Who wrote webviewhs?

(C) 2018 David Lettier  
[lettier.com](https://lettier.com)

## Who wrote webview?

Copyright (c) 2017 Serge Zaitsev
