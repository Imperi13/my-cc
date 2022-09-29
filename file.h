#pragma once

typedef enum FileType { C_SOURCE, ASSEBLER_SOURCE, OBJECT_FILE } FileType;
typedef struct FilePathList FilePathList;

#include <stdbool.h>

struct FilePathList {
  char *path;
  FilePathList *next;
};

char *read_file(char *path);
FileType get_file_type(char *path);
bool file_exists(char *filepath);
char *rename_file_ext(char *path, FileType file_type);
char *get_caronical_path(char *path);
char *get_file_directory(char *path);
