#pragma once

typedef enum TreeKind {
  FUNC_DEF,
  DECLARATION,
  INITIALIZE_LIST,
  TYPE_NAME,
  COMPOUND_STMT,
  LABEL,
  CASE,
  DEFAULT,
  RETURN,
  GOTO,
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
  CAST,
  PLUS,
  MINUS,
  ADDR,
  DEREF,
  LOGICAL_NOT,
  BIT_NOT,
  SIZEOF,
  ALIGNOF,
  FUNC_CALL,
  POST_INCREMENT,
  POST_DECREMENT,
  DOT,
  ARROW,
  NUM,
  STR,
  VAR,
  BUILTIN_VA_START,
  BUILTIN_VA_END,
} TreeKind;

typedef enum TypeSpecKind {
  // INT = 0 ,this makes the default type of no type-spec obj INT
  TypeSpec_INT,

  TypeSpec_VOID,
  TypeSpec_BOOL,
  TypeSpec_CHAR,
  TypeSpec_LONG,
  TypeSpec_TYPEDEF_NAME,
  TypeSpec_STRUCT,
  TypeSpec_UNION,
  TypeSpec_ENUM,
} TypeSpecKind;

typedef enum TypeSuffixKind {
  NONE,
  ARRAY_DECLARATOR,
  FUNC_DECLARATOR,
} TypeSuffixKind;

typedef struct Tree Tree;
typedef struct InitializeList InitializeList;
typedef struct Case Case;
typedef struct DeclSpec DeclSpec;
typedef struct StructSpec StructSpec;
typedef struct UnionSpec UnionSpec;
typedef struct EnumSpec EnumSpec;
typedef struct EnumVal EnumVal;
typedef struct Declarator Declarator;
typedef struct Pointer Pointer;
typedef struct ArrayDeclarator ArrayDeclarator;

#include <stdbool.h>

#include "analyze.h"
#include "tokenize.h"
#include "type.h"

struct Tree {
  TreeKind kind;
  Type *type;
  Token *error_token;

  // for FUNC_DEF,DECLARATION
  DeclSpec *decl_specs;
  Declarator *declarator;

  // for INITIALIZE_LIST
  InitializeList *init_list;

  // for FUNC_DEF
  Tree *func_body;

  // for argument
  bool has_variable_arg;
  int nth_arg;

  // for compound_stmt
  Tree *stmts;

  // for LABEL goto-stmt
  char *label_name;

  // for CASE
  Tree *case_num_node;
  int case_num;

  // for selection_stmt
  Tree *cond;
  int label_number;

  // for switch-stmt
  Case *cases;
  bool has_default;

  // for for-stmt
  Tree *for_init;
  Tree *for_update;

  // for cast
  Tree *type_name;
  bool is_implicit;

  // for unary and binary op
  Tree *lhs; // for unary
  Tree *rhs;

  // for func-call
  Tree *call_args;

  // for const-val
  int num;
  bool is_long;
  StrLiteral *str_literal;

  // for member
  char *member_name;

  Member *member;

  // for var
  char *var_name;

  Obj *var_obj;

  // for linked-list
  Tree *next;
};

struct InitializeList {
  int nth;
  char *member_name;
  Tree *init_val;
  // for linked-list
  InitializeList *next;
};

struct Case {
  int case_num;
  Case *next;
};

struct DeclSpec {
  // type_qual
  bool has_const;    // unused;
  bool has_volatile; // unused;
  bool has_extern;
  bool has_static;
  bool has_typedef;

  // func-spec
  bool has_inline;   // unused
  bool has_noreturn; // unused;

  // type_spec
  TypeSpecKind type_spec_kind;

  StructSpec *st_spec;
  StructDef *st_def;
  UnionSpec *union_spec;
  UnionDef *union_def;
  EnumSpec *en_spec;
  EnumDef *en_def;

  // for defined_type
  char *def_name;
  Typedef *defined_type;
};

struct StructSpec {
  char *st_name;

  bool has_decl;

  Tree *members;
};

struct UnionSpec {
  char *union_name;

  bool has_decl;
  Tree *members;
};

struct EnumSpec {
  char *en_name;

  bool has_decl;
  EnumVal *members;
};

struct EnumVal {
  char *name;

  int val;
  Tree *val_expr;
  // for linked-list
  EnumVal *next;
};

struct Declarator {
  Pointer *pointer;
  char *name;
  Token *error_token;

  Obj *def_obj;

  // for nested-declarator
  Declarator *nest;

  // for type-suffix
  TypeSuffixKind type_suffix_kind;

  // for FUNC_DECLARATOR
  Tree *args;
  bool has_arg_type;
  bool has_variable_arg;

  // for ARRAY_DECLARATOR
  ArrayDeclarator *arr_decl;

  // for init-expr
  Tree *init_expr;

  // for struct-declarator
  bool has_bitfield;
  Tree *bitfield_expr;
  int bitfield;

  // for linked-list
  Declarator *next;
};

struct Pointer {
  bool has_const; // unused
  Pointer *nest;
};

struct ArrayDeclarator {
  bool is_null_size;
  Tree *size_expr;

  // for linked-list
  ArrayDeclarator *next;
};

Tree *parse_translation_unit(Token *tok);
