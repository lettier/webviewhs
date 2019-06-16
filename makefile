# webview
# (C) 2018 David Lettier
# lettier.com

.RECIPEPREFIX != ps

_WEBVIEWHS_VERSION="0.1.0.0"
_STACK=stack --allow-different-user
_GHC_VERSION=`$(_STACK) ghc -- --version | sed 's|The Glorious Glasgow Haskell Compilation System, version ||g'`
_STACK_PATH_LOCAL_BIN=`$(_STACK) path --local-bin`
_STACK_GHC_EXE=`$(_STACK) path --compiler-exe`
_STACK_GHC_BIN=`$(_STACK) path --compiler-bin`
_STACK_PATHS=$(_STACK_PATH_LOCAL_BIN):$(_STACK_GHC_BIN)
_CABAL=env PATH=$(PATH):$(_STACK_PATHS) $(_STACK_PATH_LOCAL_BIN)/cabal
_CABAL_SANDBOX_DIR=".cabal-sandbox"

export PATH := $(PATH):$(_STACK_PATH_LOCAL_BIN)

all: setup cabal_update cabal_sandbox_clean cabal_clean cabal_install_dependencies cabal_configure cabal_build cabal_install

none:

setup: none
  $(_STACK) setup && $(_STACK) update && \
  $(_STACK) install Cabal && \
  $(_STACK) install cabal-install

cabal_sandbox: setup
  $(_CABAL) sandbox init

cabal_clean: setup
  $(_CABAL) clean

cabal_check: setup
  $(_CABAL) check

cabal_sandbox_clean: setup
  $(_CABAL) sandbox init && $(_CABAL) sandbox delete && $(_CABAL) sandbox init

cabal_update: cabal_sandbox
  $(_CABAL) --require-sandbox update

cabal_install_dependencies: cabal_sandbox
  $(_CABAL) --require-sandbox install -j -w $(_STACK_GHC_EXE) --only-dependencies

cabal_configure: cabal_sandbox
  $(_CABAL) --require-sandbox configure -w $(_STACK_GHC_EXE)

cabal_build: cabal_configure
  $(_CABAL) --require-sandbox build -j

cabal_install: cabal_build
  $(_CABAL) --require-sandbox install -j -w $(_STACK_GHC_EXE) --enable-relocatable

sdist: check build
  $(_CABAL) sdist

build_docs_directory: setup
  $(_STACK) clean && \
  $(_STACK) haddock && \
  mkdir -p docs && \
  rm -Ir docs && \
  mkdir -p docs && \
  cp -R `find '.stack-work/' -path '.stack-work/dist*doc*html*webviewhs'`/. docs/ && \
  rm -Ir docs/webviewhs.haddock && \
  rm -Ir docs/webviewhs.txt && \
  sed -i "s|\"index.html\"|\"iindex.html\"|g" docs/Graphics-UI-Webviewhs.html && \
  sed -i "s|\"index.html\"|\"iindex.html\"|g" docs/doc-index.html && \
  mv docs/index.html docs/iindex.html && \
  cp docs/Graphics-UI-Webviewhs.html docs/index.html

cabal_build_hackage_docs_archive: setup
  mkdir -p hackage-docs-archive && \
  rm -Ir hackage-docs-archive && \
  mkdir -p hackage-docs-archive && \
  $(_CABAL) --require-sandbox configure -w $(_STACK_GHC_EXE) --builddir="hackage-docs-archive" && \
  $(_CABAL) --require-sandbox haddock --with-ghc $(_STACK_GHC_EXE) \
    --builddir="hackage-docs-archive" --for-hackage --haddock-option=--hyperlinked-source && \
  cd ./hackage-docs-archive/doc/html && \
  tar --format=ustar -cvf ./webviewhs-$(_WEBVIEWHS_VERSION)-docs.tar webviewhs-$(_WEBVIEWHS_VERSION)-docs && \
  mv webviewhs-$(_WEBVIEWHS_VERSION)-docs.tar ../../
