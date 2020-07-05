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

webview_t c_create_window(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable
);

void c_bind_callback(
  webview_t w,
  const char *name,
  void (*webview_callback_fn)(const char *seq, const char *req, void *arg),
  void *arg
);

void c_return_response(
  webview_t w,
  const char *seq,
  int status,
  const char *result
);

void c_set_window_title(
  webview_t w,
  const char* newTitle
);

void c_set_window_fullscreen(
  webview_t w,
  int fullscreen
);

void c_set_window_background_color(
  webview_t w,
  uint8_t red,
  uint8_t blue,
  uint8_t green,
  uint8_t alpha
);

int c_run_javascript(
  webview_t w,
  char* javascript
);

int c_inject_css(
  webview_t w,
  const char* css
);

void c_dispatch_to_main(
  webview_t w,
  void (*webview_dispatch_fn)(webview_t w, void* arg),
  void* arg
);

void c_iterate_window(
  webview_t w
);

void c_terminate_window_loop(
  webview_t w
);

void c_destroy_window(
  webview_t w
);

#endif
