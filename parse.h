#pragma once

#include <stdbool.h>

typedef enum {
  FUNC_DEF,
  DECLARATION,
} TreeKind;

typedef struct Tree Tree;
typedef struct DeclSpec DeclSpec;
typedef struct Declarator Declarator;

struct Tree {
  TreeKind kind;

  // for FUNC_DEF,DECLARATION
  DeclSpec *decl_specs;
  Declarator *declarator;

  // for FUNC_DEF
  Tree *func_body;

  // for linked-list
  Tree *next;
};

struct DeclSpec {
  bool has_int;
};

struct Declarator {
};
