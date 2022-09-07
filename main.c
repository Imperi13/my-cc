
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

  if (optind == argc)
    error("not exist input file");

  for (int filepath_index = optind; filepath_index != argc; filepath_index++) {
    char *filename = argv[filepath_index];
    FileType file_type = get_file_type(filename);

    if (file_type != C_SOURCE)
      not_implemented("not implement expect c source file");

    char *fullpath = get_caronical_path(filename);
    char *user_input = read_file(fullpath);
    Token *token = tokenize(user_input, fullpath);

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
  }
  return 0;
}
