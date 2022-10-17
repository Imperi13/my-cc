
#include <errno.h>
#include <linux/limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "error.h"
#include "file.h"

char *read_file(char *path) {
  FILE *fp = fopen(path, "r");
  if (!fp)
    error("cannot open %s: %s", path, strerror(errno));

  if (fseek(fp, 0, SEEK_END) == -1)
    error("%s: fseek: %s", path, strerror(errno));
  size_t size = ftell(fp);
  if (fseek(fp, 0, SEEK_SET) == -1)
    error("%s: fseek: %s", path, strerror(errno));

  char *buf = calloc(size + 2, sizeof(char));
  size_t read_byte = fread(buf, 1, size, fp);
  if (read_byte < size)
    error("failed to read");

  if (size == 0 || buf[size - 1] != '\n')
    buf[size++] = '\n';
  buf[size] = '\0';
  fclose(fp);
  return buf;
}

FileType get_file_type(char *path) {
  char *dot = strrchr(path, '.');
  if (strcmp(dot, ".c") == 0)
    return C_SOURCE;
  else if (strcmp(dot, ".s") == 0)
    return ASSEBLER_SOURCE;
  else if (strcmp(dot, ".o") == 0)
    return OBJECT_FILE;
  else
    error("invalid file type");
}

bool file_exists(char *filepath) {
  struct stat tmp;
  return stat(filepath, &tmp) == 0;
}

char *rename_file_ext(char *path, FileType file_type) {
  char *dot = strrchr(path, '.');

  int filename_len = dot - path;
  char *renamed_path = calloc(filename_len + 3, sizeof(char));
  strncpy(renamed_path, path, filename_len + 1);

  if (file_type == C_SOURCE)
    strncpy(renamed_path + filename_len, ".c", 3);
  else if (file_type == ASSEBLER_SOURCE)
    strncpy(renamed_path + filename_len, ".s", 3);
  else if (file_type == OBJECT_FILE)
    strncpy(renamed_path + filename_len, ".o", 3);

  return renamed_path;
}

char *get_caronical_path(char *path) {
  char abs_path[2 * PATH_MAX + 1];
  if (path[0] == '/') {
    strncpy(abs_path, path, PATH_MAX + 1);
  } else {
    char cwd[PATH_MAX + 1];
    if (!getcwd(cwd, PATH_MAX + 1))
      error("cannot get cwd");

    snprintf(abs_path, 2 * PATH_MAX, "%s/%s", cwd, path);
  }
  char *caronical_path = calloc(PATH_MAX + 1, sizeof(char));

  if (!realpath(abs_path, caronical_path))
    error("cannot normalized path: %s", strerror(errno));

  return caronical_path;
}

char *get_file_directory(char *path) {
  assert(path[0] == '/', "path must be absolute path");

  int dir_len = strrchr(path, '/') - path;
  char *dir_buf = calloc(dir_len + 2, sizeof(char));
  strncpy(dir_buf, path, dir_len + 1);
  return dir_buf;
}
