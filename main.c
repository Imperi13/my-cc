
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

char *filename;
char *user_input;

Type *type_void;
Type *type_int;
Type *type_char;

// for avoiding global-var init
void init() {
  type_void = calloc(1, sizeof(Type));
  type_void->kind = VOID;
  type_int = calloc(1, sizeof(Type));
  type_int->kind = INT;
  type_char = calloc(1, sizeof(Type));
  type_char->kind = CHAR;

  
  variable_letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
  str_literals = 0;
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
