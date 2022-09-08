#pragma once

#include <stdbool.h>

typedef struct CommandOptions CommandOptions;
struct CommandOptions {
  bool only_preprocess;
  bool only_compile;

  char *output_file;

  int input_file_cnt;
  char **input_files;
};

CommandOptions *parse_cmd_opt(int argc, char **argv);
