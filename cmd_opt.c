
#include <stdlib.h>
#include <string.h>

#include "cmd_opt.h"
#include "error.h"

CommandOptions *parse_cmd_opt(int argc, char **argv) {
  CommandOptions *cmd_opt = calloc(1, sizeof(CommandOptions));
  cmd_opt->input_file_cnt = 0;

  for (int i = 1; i < argc; i++) {
    char *arg = argv[i];
    if (*arg != '-')
      cmd_opt->input_file_cnt++;
  }

  cmd_opt->input_files = calloc(cmd_opt->input_file_cnt + 1, sizeof(char *));

  cmd_opt->input_file_cnt = 0;

  for (int i = 1; i < argc; i++) {
    char *arg = argv[i];
    if (*arg != '-') {
      cmd_opt->input_files[cmd_opt->input_file_cnt] = arg;
      cmd_opt->input_file_cnt++;
    } else if (strncmp(arg, "-E", 2) == 0) {
      cmd_opt->only_preprocess = true;
    } else {
      not_implemented(__func__);
    }
  }

  return cmd_opt;
}
