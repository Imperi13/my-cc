#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "file.h"
#include "tokenize.h"

#ifndef __STDC__

#include "selfhost_util.h"

int fprintf();
int vfprintf();
int vsnprintf();
void exit();

#endif

void warn(char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);
  fprintf(stderr, "\e[33m[warn]\e[m ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  __builtin_va_end(ap);
}

void warn_token(Token *tok, char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);

  char *line = tok->str;
  while (tok->file_buf < line && line[-1] != '\n')
    line--;
  char *end = tok->str;
  while (*end != '\n')
    end++;

  int line_num = 1;
  for (char *p = tok->file_buf; p < line; p++)
    if (*p == '\n')
      line_num++;

  fprintf(stderr, "\e[33m[warn]\e[m");
  int indent = fprintf(stderr, " %s:%d: ", tok->filepath, line_num);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  int pos = tok->str - line + 6 + indent;
  fprintf(stderr, "%*s", pos, "");

  char msg[0x100];
  vsnprintf(msg, 0xff, fmt, ap);

  fprintf(stderr, "^ %s\n", msg);
  __builtin_va_end(ap);
}

void error(char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);
  fprintf(stderr, "\e[31m[error]\e[m ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  __builtin_va_end(ap);
  exit(1);
}

void error_token(Token *tok, char *fmt, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);

  char *line = tok->str;
  while (tok->file_buf < line && line[-1] != '\n')
    line--;
  char *end = tok->str;
  while (*end != '\n')
    end++;

  int line_num = 1;
  for (char *p = tok->file_buf; p < line; p++)
    if (*p == '\n')
      line_num++;

  fprintf(stderr, "\e[31m[error]\e[m");
  int indent = fprintf(stderr, " %s:%d: ", tok->filepath, line_num);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  int pos = tok->str - line + 7 + indent;
  fprintf(stderr, "%*s", pos, "");

  char msg[0x100];
  vsnprintf(msg, 0xff, fmt, ap);

  fprintf(stderr, "^ %s\n", msg);

  __builtin_va_end(ap);
  exit(1);
}

void not_implemented(const char *msg) { error("not implemented: %s", msg); }

void not_implemented_token(Token *tok) { error_token(tok, "not implemented"); }
