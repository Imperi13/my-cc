#pragma once

#include <stdbool.h>

#include "tokenize.h"

typedef enum {
  FUNC_DEF,
  DECLARATION,
  COMPOUND_STMT,
  RETURN,
  COMMA,
  BIT_OR,
  BIT_XOR,
  BIT_AND,
  EQUAL,
  NOT_EQUAL,
  SMALLER,
  SMALLER_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LSHIFT,
  RSHIFT,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  NUM,
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

  // for unary and binary op
  Tree *lhs; // for unary
  Tree *rhs;

  // for compound_stmt
  Tree *stmts;

  // for const-val
  unsigned long num;

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

Tree *parse_translation_unit(Token *tok);
