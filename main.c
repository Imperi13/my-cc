
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef __STDC__

#include "selfhost_util.h"
void *calloc();
int getopt();
int fprintf();

#endif

#include "analyze.h"
#include "codegen.h"
#include "error.h"
#include "file.h"
#include "parse.h"
#include "preprocess.h"
#include "tokenize.h"
#include "type.h"

typedef struct CommandOptions CommandOptions;
struct CommandOptions {
  bool only_preprocess;
};

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

  int c;
  opterr = 0;

  CommandOptions *cmd_opt = calloc(1, sizeof(CommandOptions));

  while ((c = getopt(argc, argv, "E")) != -1) {
    if (c == 'E') {
      cmd_opt->only_preprocess = true;
    } else {
      fprintf(stderr, "opt=%c\n", optopt);
      error("invalid option");
    }
  }

  if (optind + 1 != argc)
    error("invalid argv");

  init();

  char *filename = argv[optind];
  char *user_input = read_file(filename);
  Token *token = tokenize(user_input, filename);

  //  debug_token(token);

  token = preprocess(token);

  if (cmd_opt->only_preprocess) {
    print_token_seq(stdout, token);
    return 0;
  }

  token = remove_newline(token);

  Tree *ast = parse_translation_unit(token);

  analyze_translation_unit(ast);

  codegen_translation_unit(stdout, ast);
  // codegen_all(stdout);
  return 0;
}
