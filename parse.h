#pragma once

#include <stdbool.h>

typedef enum {
  FUNC_DEF,
  DECLARATION,
  TYPE_NAME,
  COMPOUND_STMT,
  LABEL,
  DEFAULT,
  RETURN,
  BREAK,
  CONTINUE,
  DO_WHILE,
  WHILE,
  FOR,
  IF,
  SWITCH,
  COMMA,
  ASSIGN,
  ADD_ASSIGN,
  SUB_ASSIGN,
  MUL_ASSIGN,
  DIV_ASSIGN,
  MOD_ASSIGN,
  AND_ASSIGN,
  OR_ASSIGN,
  XOR_ASSIGN,
  LSHIFT_ASSIGN,
  RSHIFT_ASSIGN,
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
  ADDR,
  DEREF,
  LOGICAL_NOT,
  BIT_NOT,
  SIZEOF,
  FUNC_CALL,
  POST_INCREMENT,
  POST_DECREMENT,
  DOT,
  ARROW,
  NUM,
  STR,
  VAR,
} TreeKind;

typedef struct Tree Tree;
typedef struct DeclSpec DeclSpec;
typedef struct StructSpec StructSpec;
typedef struct Declarator Declarator;
typedef struct Pointer Pointer;
typedef struct ArrayDeclarator ArrayDeclarator;

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

  // for LABEL
  char *label_name;
  int label_len;

  // for selection_stmt
  Tree *cond;
  int label_number;
  
  // for switch-stmt
  bool has_default;

  // for for-stmt
  Tree *for_init;
  Tree *for_update;

  // for unary and binary op
  Tree *lhs; // for unary
  Tree *rhs;

  // for compound_stmt
  Tree *stmts;

  // for func-call
  Tree *call_args;

  // for const-val
  unsigned long num;
  StrLiteral *str_literal;

  // for member
  char *member_name;
  int member_len;

  Member *member;

  // for var
  char *var_name;
  int var_len;

  Obj *var_obj;

  // for linked-list
  Tree *next;
};

struct DeclSpec {
  bool has_void;
  bool has_int;
  bool has_char;
  StructSpec *st_spec;
  StructDef *st_def;
};

struct StructSpec {
  char *st_name;
  int st_len;

  bool has_decl;

  Tree *members;
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

  // for ARRAY_DECLARATOR
  ArrayDeclarator *arr_decl;
};

struct Pointer {
  Pointer *nest;
};

struct ArrayDeclarator {
  Tree *size;

  // for linked-list
  ArrayDeclarator *next;
};

Tree *parse_translation_unit(Token *tok);
