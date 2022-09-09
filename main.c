
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "analyze.h"
#include "cmd_opt.h"
#include "codegen.h"
#include "error.h"
#include "file.h"
#include "parse.h"
#include "preprocess.h"
#include "tokenize.h"

int main(int argc, char **argv) {

  CommandOptions *cmd_opt = parse_cmd_opt(argc, argv);

  if (cmd_opt->input_file_cnt > 1 && cmd_opt->output_file &&
      (cmd_opt->only_preprocess))
    error("cannot output for multiple files");

  for (int input_index = 0; input_index < cmd_opt->input_file_cnt;
       input_index++) {
    char *filename = cmd_opt->input_files[input_index];
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
      continue;
    }

    token = remove_newline(token);

    Tree *ast = parse_translation_unit(token);

    analyze_translation_unit(ast);

    FILE *output = fopen((cmd_opt->output_file
                              ? cmd_opt->output_file
                              : rename_file_ext(filename, ASSEBLER_SOURCE)),
                         "w");

    codegen_translation_unit(output, ast);
    // codegen_all(stdout);

    fclose(output);
  }
  return 0;
}
