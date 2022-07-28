#pragma once

#include <stdbool.h>

typedef enum {
  FUNC_DEF,
  DECLARATION,
  COMPOUND_STMT,
  RETURN,
  COMMA,
  ASSIGN,
  ADD_ASSIGN,
  SUB_ASSIGN,
  MUL_ASSIGN,
  DIV_ASSIGN,
  CONDITIONAL,
  LOGICAL_OR,
  LOGICAL_AND,
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
  PLUS,
  MINUS,
  LOGICAL_NOT,
  BIT_NOT,
  FUNC_CALL,
  NUM,
  VAR,
} TreeKind;

typedef struct Tree Tree;
typedef struct DeclSpec DeclSpec;
typedef struct Declarator Declarator;
typedef struct Pointer Pointer;

#include "analyze.h"
#include "tokenize.h"
#include "type.h"

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

  Obj *def_obj;

  // for FUNC_DEF
  Tree *func_body;

  Type *type;

  // for conditional
  Tree *cond;
  int label_number;

  // for unary and binary op
  Tree *lhs; // for unary
  Tree *rhs;

  // for compound_stmt
  Tree *stmts;

  // for func-call
  Tree *call_args;

  // for const-val
  unsigned long num;

  // for var
  char *var_name;
  int var_len;

  Obj *var_obj;

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
  Tree *args;
};

struct Pointer {};

Tree *parse_translation_unit(Token *tok);
