#pragma once

typedef enum FileType { C_SOURCE, ASSEBLER_SOURCE, OBJECT_FILE } FileType;

char *read_file(char *path);
FileType get_file_type(char *path);
char *get_caronical_path(char *path);
