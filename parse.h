#pragma once

#include <stdbool.h>

typedef enum {
  FUNC_DEF,
  DECLARATION,
} TreeKind;

typedef struct Tree Tree;
typedef struct DeclSpec DeclSpec;
typedef struct Declarator Declarator;
typedef struct Pointer Pointer;
typedef struct Argument Argument;

typedef enum {
  NONE,
  ARRAY_DECLARATOR,
  FUNC_DECLARATOR,
} TypeSuffixKind;

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
  Pointer *pointer;
  char *name;
  int len;

  // for nested-declarator
  Declarator *nest;

  // for type-suffix
  TypeSuffixKind type_suffix_kind;

  // for FUNC_DECLARATOR
  Argument *arg;
};

struct Pointer {};
