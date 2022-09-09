
#include <stdlib.h>
#include <string.h>

#include "cmd_opt.h"
#include "error.h"

CommandOptions *parse_cmd_opt(int argc, char **argv) {
  CommandOptions *cmd_opt = calloc(1, sizeof(CommandOptions));
  cmd_opt->input_file_cnt = 0;

  int i = 1;

  while (i < argc) {
    char *arg = argv[i];

    if (*arg != '-') {
      cmd_opt->input_file_cnt++;
      i++;
    } else if (strncmp(arg, "-o", 2) == 0) {
      i++;
      if (*argv[i] == '-')
        error("not exist output filename");
      i++;
    } else {
      i++;
    }
  }

  cmd_opt->input_files = calloc(cmd_opt->input_file_cnt + 1, sizeof(char *));

  cmd_opt->input_file_cnt = 0;

  i = 1;
  while (i < argc) {
    char *arg = argv[i];

    if (*arg != '-') {
      cmd_opt->input_files[cmd_opt->input_file_cnt] = arg;
      cmd_opt->input_file_cnt++;

      i++;
    } else if (strncmp(arg, "-o", 2) == 0) {
      i++;

      cmd_opt->output_file = argv[i];
      i++;
    } else if (strncmp(arg, "-E", 2) == 0) {
      cmd_opt->only_preprocess = true;
      i++;
    } else if (strncmp(arg, "-S", 2) == 0) {
      cmd_opt->only_compile = true;
      i++;
    } else if (strncmp(arg, "-c", 2) == 0) {
      cmd_opt->only_assemble = true;
      i++;
    } else {
      not_implemented(__func__);
    }
  }

  return cmd_opt;
}
