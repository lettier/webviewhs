![webviewhs logo](https://i.imgur.com/yohrYgX.png)

# How do I use webviewhs?

- [How do I create a window and have it run itself?](how-do-i-create-a-window-and-have-it-run-itself.hs)
- [How do I create a window?](how-do-i-create-a-window.hs)
- [How do I set the window title?](how-do-i-set-the-window-title.hs)
- [How do I toggle the window's fullscreen state?](how-do-i-toggle-the-windows-fullscreen-state.hs)
- [How do I set the window's color?](how-do-i-set-the-windows-color.hs)
- [How do I terminate the window loop?](how-do-i-terminate-the-window-loop.hs)
- [How do I exit the window?](how-do-i-exit-the-window.hs)
- [How do I run my own function in the window loop?](how-do-i-run-my-own-function-in-the-window-loop.hs)
- [How do I run some function in the main window thread while in some other thread?](how-do-i-run-some-function-in-the-main-window-thread-while-in-some-other-thread.hs)
- [How do I open an alert dialog?](how-do-i-open-an-alert-dialog.hs)
- [How do I open an open dialog?](how-do-i-open-an-open-dialog.hs)
- [How do I open a save dialog?](how-do-i-open-a-save-dialog.hs)
- [How do I run some JavaScript inside the window?](how-do-i-run-some-javascript-inside-the-window.hs)
- [How do I communicate with Haskell from JavaScript?](how-do-i-communicate-with-haskell-from-javascript.hs)
- [How do I inline an HTML page on start up?](how-do-i-inline-an-html-page-on-start-up.hs)
- [How do I inject some custom CSS into the Window?](how-do-i-inject-some-custom-css-into-the-window.hs)
- [How do I log some debug information?](how-do-i-log-some-debug-information.hs)

## How do I build and run these examples?

If you're using stack:

```bash
stack build && \
stack exec -- how-do-i-create-a-window-and-have-it-run-itself && \
stack exec -- how-do-i-create-a-window && \
stack exec -- how-do-i-set-the-window-title && \
stack exec -- how-do-i-toggle-the-windows-fullscreen-state && \
stack exec -- how-do-i-set-the-windows-color && \
stack exec -- how-do-i-terminate-the-window-loop && \
stack exec -- how-do-i-exit-the-window && \
stack exec -- how-do-i-run-my-own-function-in-the-window-loop && \
stack exec -- how-do-i-run-some-function-in-the-main-window-thread-while-in-some-other-thread && \
stack exec -- how-do-i-open-an-alert-dialog && \
stack exec -- how-do-i-open-an-open-dialog && \
stack exec -- how-do-i-open-a-save-dialog && \
stack exec -- how-do-i-run-some-javascript-inside-the-window && \
stack exec -- how-do-i-communicate-with-haskell-from-javascript && \
stack exec -- how-do-i-inline-an-html-page-on-start-up && \
stack exec -- how-do-i-inject-some-custom-css-into-the-window && \
stack exec -- how-do-i-log-some-debug-information
```

If you're using cabal:

```bash
cabal sandbox init && \
cabal sandbox add-source ../ && \
cabal --require-sandbox install -j && \
cabal --require-sandbox exec -- how-do-i-create-a-window-and-have-it-run-itself && \
cabal --require-sandbox exec -- how-do-i-create-a-window && \
cabal --require-sandbox exec -- how-do-i-set-the-window-title && \
cabal --require-sandbox exec -- how-do-i-toggle-the-windows-fullscreen-state && \
cabal --require-sandbox exec -- how-do-i-set-the-windows-color && \
cabal --require-sandbox exec -- how-do-i-terminate-the-window-loop && \
cabal --require-sandbox exec -- how-do-i-exit-the-window && \
cabal --require-sandbox exec -- how-do-i-run-my-own-function-in-the-window-loop && \
cabal --require-sandbox exec -- how-do-i-run-some-function-in-the-main-window-thread-while-in-some-other-thread && \
cabal --require-sandbox exec -- how-do-i-open-an-alert-dialog && \
cabal --require-sandbox exec -- how-do-i-open-an-open-dialog && \
cabal --require-sandbox exec -- how-do-i-open-a-save-dialog && \
cabal --require-sandbox exec -- how-do-i-run-some-javascript-inside-the-window && \
cabal --require-sandbox exec -- how-do-i-communicate-with-haskell-from-javascript && \
cabal --require-sandbox exec -- how-do-i-inline-an-html-page-on-start-up && \
cabal --require-sandbox exec -- how-do-i-inject-some-custom-css-into-the-window && \
cabal --require-sandbox exec -- how-do-i-log-some-debug-information
```

## Who wrote webviewhs?

(C) 2018 David Lettier  
[lettier.com](https://lettier.com)

## Who wrote webview?

Copyright (c) 2017 Serge Zaitsev
