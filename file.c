

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
  fread(buf, size, 1, fp);

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

char *get_caronical_path(char *path) {
  char abs_path[2 * PATH_MAX + 1];
  if (path[0] == '/') {
    strncpy(abs_path, path, strlen(path));
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
