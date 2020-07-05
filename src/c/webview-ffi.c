/*
  webviewhs
  (C) 2018 David Lettier
  lettier.com
*/

#define WEBVIEW_IMPLEMENTATION

#include "webview.h"
#include "webview-ffi.h"

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define CSS_INJECT_FUNCTION                                                    \
  "(function(e){var "                                                          \
  "t=document.createElement('style'),d=document.head||document."               \
  "getElementsByTagName('head')[0];t.setAttribute('type','text/"               \
  "css'),t.styleSheet?t.styleSheet.cssText=e:t.appendChild(document."          \
  "createTextNode(e)),d.appendChild(t)})"

static int webview_js_encode(const char *s, char *esc, size_t n) {
  int r = 1; /* At least one byte for trailing zero */
  for (; *s; s++) {
    const unsigned char c = *s;
    if (c >= 0x20 && c < 0x80 && strchr("<>\\'\"", c) == NULL) {
      if (n > 0) {
        *esc++ = c;
        n--;
      }
      r++;
    } else {
      if (n > 0) {
        snprintf(esc, n, "\\x%02x", (int)c);
        esc += 4;
        n -= 4;
      }
      r += 4;
    }
  }
  return r;
}

void c_create_window_and_block(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable
) {
  webview_t w = c_create_window(title, uri, width, height, resizable, debuggable);
  c_iterate_window(w);
  c_destroy_window(w);
}

webview_t c_create_window(
  const char* title,
  const char* uri,
  int width,
  int height,
  int resizable,
  int debuggable
) {
  webview_t w;
  w = malloc(sizeof(webview_t));
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not create a window!\n");
    return NULL;
  }
  w = webview_create(debuggable, NULL);
  c_set_window_title(w, title);
  webview_set_size(w, width, height,
    resizable ? WEBVIEW_HINT_NONE : WEBVIEW_HINT_FIXED);
  webview_navigate(w, uri);
  return w;
}

void c_bind_callback(
  webview_t w,
  const char *name,
  void (*webview_callback_fn)(const char *seq, const char *req, void *arg),
  void *arg
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not bind the callback!\n");
    return;
  }
  webview_bind(w, name, webview_callback_fn, arg);
}

void c_return_response(
  webview_t w,
  const char *seq,
  int status,
  const char *result
) {
    if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not return the response!\n");
    return;
  }
  webview_return(w, seq, status, result);
}

void c_set_window_title(
  webview_t w,
  const char* newTitle
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not set the window title!\n");
    return;
  }
  webview_set_title(w, newTitle);
}

void c_iterate_window(
  webview_t w
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not iterate the window!\n");
    return;
  }
  webview_run(w);
}

int c_run_javascript(
  webview_t w,
  char* javascript
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not run JavaScript!\n");
    return -1;
  }
  webview_eval(w, javascript);
  return 1;
}

int c_inject_css(
  webview_t w,
  const char* css
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not inject CSS!\n");
    return -1;
  }
  int n = webview_js_encode(css, NULL, 0);
  char *esc = (char *)calloc(1, sizeof(CSS_INJECT_FUNCTION) + n + 4);
  if (esc == NULL) {
    return -1;
  }
  char *js = (char *)calloc(1, n);
  webview_js_encode(css, js, n);
  snprintf(esc, sizeof(CSS_INJECT_FUNCTION) + n + 4, "%s(\"%s\")",
           CSS_INJECT_FUNCTION, js);
  webview_eval(w, esc);
  free(js);
  free(esc);
  return 1;
}

void c_dispatch_to_main(
  webview_t w,
  void (*webview_dispatch_fn)(webview_t w, void *arg),
  void* arg
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not dispatch to main!\n");
    return;
  }
  webview_dispatch(w, webview_dispatch_fn, arg);
}

void c_terminate_window_loop(
  webview_t w
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not terminate the window loop!\n");
    return;
  }
  webview_terminate(w);
}

void c_destroy_window(
  webview_t w
) {
  if (NULL == w) {
    printf("[WEBVIEWHS:ERROR] Could not destroy the window!\n");
    return;
  }
  webview_destroy(w);
  w = NULL;
}
