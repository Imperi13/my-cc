#pragma once

#include "tokenize.h"
#include "file.h"

extern FilePathList *include_path_list;

Token *preprocess(Token *tok);
Token *remove_newline(Token *tok);
