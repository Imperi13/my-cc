#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "file.h"

// TODO 置き場

void error(char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);
  fprintf(stderr, "[error] ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  __builtin_va_end(ap);
  exit(1);
}

void error_at(char *loc, char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);

  char *line = loc;
  while (user_input < line && line[-1] != '\n')
    line--;
  char *end = loc;
  while (*end != '\n')
    end++;

  int line_num = 1;
  for (char *p = user_input; p < line; p++)
    if (*p == '\n')
      line_num++;

  int indent = fprintf(stderr, "[error] %s:%d: ", filename, line_num);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  int pos = loc - line + indent;
  fprintf(stderr, "%*s", pos, "");

  char msg[0x100];
  snprintf(msg, 0xff, fmt, ap);

  fprintf(stderr, "^ %s\n", msg);

  __builtin_va_end(ap);
  exit(1);
}

void not_implemented(const char *msg) { error("not implemented: %s", msg); }

void not_implemented_at(char *loc) { error_at(loc, "not implemented"); }

void warn(char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);
  fprintf(stderr, "[warn] ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  __builtin_va_end(ap);
}

void warn_at(char *loc, char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);

  char *line = loc;
  while (user_input < line && line[-1] != '\n')
    line--;
  char *end = loc;
  while (*end != '\n')
    end++;

  int line_num = 1;
  for (char *p = user_input; p < line; p++)
    if (*p == '\n')
      line_num++;

  int indent = fprintf(stderr, "[warn] %s:%d: ", filename, line_num);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  int pos = loc - line + indent;
  fprintf(stderr, "%*s", pos, "");

  char msg[0x100];
  snprintf(msg, 0xff, fmt, ap);

  fprintf(stderr, "^ %s\n", msg);
  __builtin_va_end(ap);
}
