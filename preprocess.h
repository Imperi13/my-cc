#pragma once

typedef struct Define Define;

#include "tokenize.h"

struct Define {
  char *name;
  int len;

  Token *start;
  Token *end;
  // for linked-list
  Define *next;
};

Token *preprocess(Token *tok);
