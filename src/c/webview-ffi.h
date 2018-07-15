/*
  webviewhs
  (C) 2018 David Lettier
  lettier.com
*/

#ifndef WEBVIEW_FFI_H
#define WEBVIEW_FFI_H

#include <stddef.h>
#include <stdint.h>

void c_create_window_and_block(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable
);

struct webview* c_create_window(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable,
  void (*webview_callback_fn)(struct webview* w, const char* arg)
);

void c_set_window_title(
  struct webview* w,
  const char* newTitle
);

void c_set_window_fullscreen(
  struct webview* w,
  int fullscreen
);

void c_set_window_background_color(
  struct webview* w,
  uint8_t red,
  uint8_t blue,
  uint8_t green,
  uint8_t alpha
);

int c_run_javascript(
  struct webview* w,
  char* javascript
);

int c_inject_css(
  struct webview* w,
  const char* css
);

void c_open_dialog(
  struct webview* w,
  enum webview_dialog_type dlgtype,
  int flags,
  const char* primary_text,
  const char* secondary_text,
  char* result,
  size_t resultsz
);

void c_dispatch_to_main(
  struct webview* w,
  void (*webview_dispatch_fn)(struct webview* w, void* arg),
  void* arg
);

void c_log(
  char* message
);

int c_iterate_window(
  struct webview* w,
  int block
);

void c_terminate_window_loop(
  struct webview* w
);

void c_destroy_window(
  struct webview* w
);

#endif
