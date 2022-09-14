#pragma once

#include "file.h"
#include "tokenize.h"

Token *preprocess(Token *tok);
Token *remove_newline(Token *tok);

void add_include_path(char *include_path);
