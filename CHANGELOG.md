# webviewhs

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
