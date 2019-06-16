![webviewhs logo](https://i.imgur.com/yohrYgX.png)

# How do I use webviewhs light?

- [How do I communicate with Haskell from JavaScript?](how-do-i-communicate-with-haskell-from-javascript.hs)
- [How do I inject some custom CSS into the Window?](how-do-i-inject-some-custom-css-into-the-window.hs)
- [How do I log some debug information?](how-do-i-log-some-debug-information.hs)

## How do I build and run these examples?

If you're using stack:

```bash
# --flag webviewhs:light is redundant as it is listed in the stack.yaml file.
# However, it is shown here for demonstration purposes.
stack build --flag webviewhs:light && \
stack exec -- how-do-i-communicate-with-haskell-from-javascript && \
stack exec -- how-do-i-inject-some-custom-css-into-the-window && \
stack exec -- how-do-i-log-some-debug-information
```

If you're using cabal:

```bash
cabal sandbox init && \
cabal sandbox add-source ../ && \
cabal --require-sandbox install -j --constraint="webviewhs +light" && \
cabal --require-sandbox exec -- how-do-i-communicate-with-haskell-from-javascript && \
cabal --require-sandbox exec -- how-do-i-inject-some-custom-css-into-the-window && \
cabal --require-sandbox exec -- how-do-i-log-some-debug-information
```

## Who wrote webviewhs?

(C) 2018 David Lettier  
[lettier.com](https://lettier.com)

## Who wrote webview?

Copyright (c) 2017 Serge Zaitsev
