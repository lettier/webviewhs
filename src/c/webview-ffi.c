/*
  webviewhs
  (C) 2018 David Lettier
  lettier.com
*/

#define WEBVIEW_IMPLEMENTATION

#include "webview.h"
#include "webview-ffi.h"

void c_create_window_and_block(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable
) {
  webview(title, uri, width, height, resizable, debuggable);
}

struct webview* c_create_window(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable,
  void (*webview_callback_fn)(struct webview* w, const char* arg)
) {
  struct webview* w;
  w = malloc(sizeof(struct webview));
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not create a window!\n");
    return NULL;
  }
  w->title              = title;
  w->url                = uri;
  w->width              = width;
  w->height             = height;
  w->debug              = debuggable;
  w->resizable          = resizable;
  w->external_invoke_cb = webview_callback_fn;
  webview_init(w);
  return w;
}

void c_set_window_title(
  struct webview* w,
  const char* newTitle
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not set the window title!\n");
    return;
  }
  webview_set_title(w, newTitle);
}

void c_set_window_fullscreen(
  struct webview* w,
  int fullscreen
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not set the window fullscreen!\n");
    return;
  }
  webview_set_fullscreen(w, fullscreen);
}

void c_set_window_background_color(
  struct webview* w,
  uint8_t red,
  uint8_t green,
  uint8_t blue,
  uint8_t alpha
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not set the window background color!\n");
    return;
  }
  webview_set_color(w, red, green, blue, alpha);
}

int c_iterate_window(
  struct webview* w,
  int block
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not iterate the window!\n");
    return -1;
  }
  if (1 == block) {
    int should_exit = 0;
    do {
      should_exit = webview_loop(w, block);
    } while (0 == should_exit);
    return should_exit;
  }
  return webview_loop(w, block);
}

int c_run_javascript(
  struct webview* w,
  char* javascript
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not run JavaScript!\n");
    return -1;
  }
  return webview_eval(w, javascript);
}

int c_inject_css(
  struct webview* w,
  const char* css
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not inject CSS!\n");
    return -1;
  }
  return webview_inject_css(w, css);
}

void c_open_window_dialog(
  struct webview* w,
  enum webview_dialog_type dlgtype,
  int flags,
  const char* primary_text,
  const char* secondary_text,
  char* result,
  size_t result_buffer_size
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not open window dialog!\n");
    return;
  }
  webview_dialog(
    w,
    dlgtype,
    flags,
    primary_text,
    secondary_text,
    result,
    result_buffer_size
  );
}

void c_dispatch_to_main(
  struct webview* w,
  void (*webview_dispatch_fn)(struct webview *ww, void *arg),
  void* arg
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not dispatch to main!\n");
    return;
  }
  webview_dispatch(w, webview_dispatch_fn, arg);
}

void c_log(
  char* message
) {
  webview_debug(message);
}

void c_terminate_window_loop(
  struct webview* w
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not terminate the window loop!\n");
    return;
  }
  webview_terminate(w);
}

void c_destroy_window(
  struct webview* w
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not destroy the window!\n");
    return;
  }
  webview_exit(w);
  free(w);
  w = NULL;
}
