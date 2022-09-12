
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include "analyze.h"
#include "cmd_opt.h"
#include "codegen.h"
#include "error.h"
#include "file.h"
#include "parse.h"
#include "preprocess.h"
#include "tokenize.h"

static void run_cmd(char **argv);
static char *create_tmpfile(FileType file_type);

void run_cmd(char **argv) {
  if (!argv || !argv[0])
    error("invalid run_cmd argv");

  pid_t pid = fork();

  if (pid == 0) {
    execvp(argv[0], argv);
    _exit(0);
  } else if (pid < 0) {
    error("failed to fork: %s", strerror(errno));
  }

  int status;
  waitpid(pid, &status, 0);

  if (!WIFEXITED(status))
    error("filed to run cmd");
}

void assemble(char *input_path, char *output_path) {
  char *argv[] = {"as", "--64", "-o", output_path, input_path, NULL};
  run_cmd(argv);
}

// use musl-gcc instead of ld because I'm not sure about the linker option
// TODO use ld linker
void linker(FilePathList *linker_list, char *output_path) {
  int cnt = 3;
  for (FilePathList *cur = linker_list; cur; cur = cur->next)
    cnt++;

  char **argv = calloc(cnt + 1, sizeof(char *));
  argv[0] = "/usr/local/musl/bin/musl-gcc";
  argv[1] = "-o";
  argv[2] = output_path;

  cnt = 3;
  for (FilePathList *cur = linker_list; cur; cur = cur->next) {
    argv[cnt] = cur->path;
    cnt++;
  }

  argv[cnt] = NULL;

  run_cmd(argv);
}

char *create_tmpfile(FileType file_type) {
  char *path = NULL;
  int fd;

  if (file_type == C_SOURCE) {
    path = strdup("/tmp/myccXXXXXX.c");
    fd = mkstemps(path, 2);
  } else if (file_type == ASSEBLER_SOURCE) {
    path = strdup("/tmp/myccXXXXXX.s");
    fd = mkstemps(path, 2);
  } else if (file_type == OBJECT_FILE) {
    path = strdup("/tmp/myccXXXXXX.o");
    fd = mkstemps(path, 2);
  } else {
    not_implemented(__func__);
  }

  close(fd);
  return path;
}

int main(int argc, char **argv) {

  CommandOptions *cmd_opt = parse_cmd_opt(argc, argv);

  if (cmd_opt->input_file_cnt > 1 && cmd_opt->output_file &&
      (cmd_opt->only_preprocess || cmd_opt->only_compile ||
       cmd_opt->only_assemble))
    error("cannot output for multiple files with -E -S -c");

  FilePathList *linker_list = NULL;

  {
    FilePathList *tmp = calloc(1, sizeof(FilePathList));
    tmp->path = "/usr/local/musl/include";
    tmp->next = include_path_list;
    include_path_list = tmp;
  }

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

    if (cmd_opt->only_compile) {
      FILE *output = fopen((cmd_opt->output_file
                                ? cmd_opt->output_file
                                : rename_file_ext(filename, ASSEBLER_SOURCE)),
                           "w");

      codegen_translation_unit(output, ast);
      fclose(output);
      continue;
    }

    char *asm_source_path = create_tmpfile(ASSEBLER_SOURCE);
    FILE *compile_output = fopen(asm_source_path, "w");
    codegen_translation_unit(compile_output, ast);
    fclose(compile_output);

    if (cmd_opt->only_assemble) {
      char *output_path =
          (cmd_opt->output_file ? cmd_opt->output_file
                                : rename_file_ext(filename, OBJECT_FILE));

      assemble(asm_source_path, output_path);
      continue;
    }

    char *object_path = create_tmpfile(OBJECT_FILE);
    assemble(asm_source_path, object_path);

    FilePathList *object_file_list = calloc(1, sizeof(FilePathList));
    object_file_list->path = object_path;
    object_file_list->next = linker_list;
    linker_list = object_file_list;
  }

  if (!cmd_opt->only_preprocess && !cmd_opt->only_compile &&
      !cmd_opt->only_assemble) {
    char *output_path = (cmd_opt->output_file ? cmd_opt->output_file : "a.out");
    linker(linker_list, output_path);
  }

  return 0;
}
