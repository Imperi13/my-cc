
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

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

  char *filename = get_caronical_path(argv[optind]);
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
