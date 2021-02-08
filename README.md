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
- [x] webview_dispatch
- [x] webview_terminate
- [x] webview_exit
- [x] webview_bind
- [x] webview_return

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

You can also bind callbacks to communicate with HTML/JS:

```haskell
{-# LANGUAGE
    OverloadedStrings
#-}

import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  eitherWindow <- WHS.createWindow windowParams
  case eitherWindow of
    Left  _      -> pure ()
    Right window -> do
      WHS.bindCallback window "callback1" callback "custom data"
      WHS.bindCallback window "callback2" callbackWithResponse ()
      WHS.iterateWindowLoop window
      WHS.terminateWindowLoop window
      WHS.destroyWindow window
  where
    windowParams = WHS.WindowParams
      { WHS.windowParamsTitle      = "Test"
      , WHS.windowParamsUri        = "https://lettier.github.io"
      , WHS.windowParamsWidth      = 800
      , WHS.windowParamsHeight     = 600
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }

    -- This can be called from Html/JS as "window.callback1(msg)"
    -- userData was given to bindCallback as parameter
    callback window _ req userData = do
      print req
      print userData

    -- This can be called from Html/JS as "let p = window.callback2(msg)"
    -- where p is a Promise which can be rejected, resolved, or ignored
    callbackWithResponse window reqId req _ = do
      print req
      WHS.respondRequest reqId WHS.RequestResolve "'some data'"
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
