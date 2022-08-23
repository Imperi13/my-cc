
#include <stdlib.h>

#ifndef __STDC__
void *calloc();
#endif

#include "analyze.h"
#include "codegen.h"
#include "error.h"
#include "file.h"
#include "parse.h"
#include "preprocess.h"
#include "tokenize.h"
#include "type.h"

char *filename;
char *user_input;

char *call_register64[6];
char *call_register32[6];
char *call_register8[6];

Type *type_void;
Type *type_long;
Type *type_int;
Type *type_char;
Type *type_bool;

// for avoiding global-var init
void init() {
  type_void = calloc(1, sizeof(Type));
  type_void->kind = VOID;
  type_long = calloc(1, sizeof(Type));
  type_long->kind = LONG;
  type_int = calloc(1, sizeof(Type));
  type_int->kind = INT;
  type_char = calloc(1, sizeof(Type));
  type_char->kind = CHAR;
  type_bool = calloc(1, sizeof(Type));
  type_bool->kind = BOOL;

  variable_letters =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

  call_register64[0] = "rdi";
  call_register64[1] = "rsi";
  call_register64[2] = "rdx";
  call_register64[3] = "rcx";
  call_register64[4] = "r8";
  call_register64[5] = "r9";

  call_register32[0] = "edi";
  call_register32[1] = "esi";
  call_register32[2] = "edx";
  call_register32[3] = "ecx";
  call_register32[4] = "r8d";
  call_register32[5] = "r9d";

  call_register8[0] = "dil";
  call_register8[1] = "sil";
  call_register8[2] = "dl";
  call_register8[3] = "cl";
  call_register8[4] = "r8b";
  call_register8[5] = "r9b";
}

int main(int argc, char **argv) {
  if (argc != 2)
    error("invalid argv");

  init();

  filename = argv[1];
  user_input = read_file(argv[1]);
  Token *token = tokenize(user_input, filename);

  // debug_token(token);

  token = preprocess(token);

  // debug_token(token);

  Tree *ast = parse_translation_unit(token);

  analyze_translation_unit(ast);

  codegen_translation_unit(ast);
  // codegen_all(stdout);
  return 0;
}
