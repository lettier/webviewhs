# webviewhs

---

## 0.1.0.0

### Added

- Cabal build flag light
  - If enabled
    - Removes
      - clay, jmacro, and text-format-heavy
      - runJavaScript, injectCss, and log from the API
- examples-light directory
- WithWindowLoopSetUp
- WithWindowLoopTearDown

### Changed

- withWindowLoop
  - It now requires a setup and teardown function
- Documentation

### Removed

-

---

## 0.0.2.0

### Added

-

### Changed

- Updated webview
  - Commits
    - Fixed bug in Windows 7 where window does not close
      - 16c93bcaeaeb6aa7bb5a1432de3bef0b9ecc44f3
      - https://github.com/zserge/webview/pull/229
    - Add IInternetSecurityManager to override any IE settings.
      - f390a2df9ec50d1bce389f0656a215a5504dce04
      - https://github.com/zserge/webview/pull/199
- Updated version in documentation

### Removed

-

---

## 0.0.1.0

### Added

-

### Changed

- Updated webview
  - Commit
    - 24af6cb9b33b3a10dce44188aec0c1646247d830
  - Removes objective-c
    - https://github.com/zserge/webview/pull/181
- Updated logo

### Removed

- Objective-c requirement for macOS
- Cocoa framework requirement for macOS

---

## 0.0.0.0

### Added

- Logo
- Docs
- Examples
- Error checking to webview_eval
- API
    - WindowParams
    - WindowBackgroundColor
    - WindowAlertDialogType
    - Window
    - createWindowAndBlock
    - createWindow
    - setWindowTitle
    - setWindowFullscreen
    - setWindowBackgroundColor
    - withWindowLoop
    - iterateWindowLoop
    - runJavaScript
    - runJavaScript'
    - injectCss
    - injectCss'
    - openWindowAlertDialog
    - withWindowOpenDialog
    - withWindowSaveDialog
    - dispatchToMain
    - log
    - log'
    - terminateWindowLoop
    - destroyWindow

### Changed

- webview interface to allow debugging
- GTK WebKit
    - webkit_settings_set_enable_webgl true
    - webkit_settings_set_javascript_can_access_clipboard true
    - webkit_settings_set_allow_file_access_from_file_urls true

### Removed

-
