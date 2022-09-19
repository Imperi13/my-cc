#pragma once

typedef enum TypeKind {
  VOID,
  LONGLONG,
  LONG,
  INT,
  SHORT,
  CHAR,
  BOOL,
  PTR,
  STRUCT,
  UNION,
  FUNC,
  ARRAY,
} TypeKind;

typedef struct Type Type;

#include "analyze.h"
#include "parse.h"
#include "vector.h"

struct Type {
  TypeKind kind;

  // for FUNC
  Type *return_type;
  bool has_arg;
  bool has_variable_arg;
  Vector *args_vector;

  // for PTR,ARRAY
  Type *ptr_to;

  // for ARRAY
  int arr_size;

  // for STRUCT
  StructDef *st_def;

  // for UNION
  UnionDef *union_def;
};

extern Type type_void;
extern Type type_longlong;
extern Type type_long;
extern Type type_int;
extern Type type_short;
extern Type type_char;
extern Type type_bool;

void builtin_type_init(Analyze *state);

Type *gettype_decl_spec(DeclSpec *decl_spec);
Type *gettype_declarator(Declarator *declarator, Type *base_type);
char *getname_declarator(Declarator *declarator);
Tree *getargs_declarator(Declarator *declarator);
ArrayDeclarator *get_arr_declarator(Declarator *declarator);

Type *newtype_ptr(Type *type);
Type *newtype_struct(StructDef *st_def);
Type *newtype_union(UnionDef *union_def);

int type_size(Type *type);
int type_alignment(Type *type);

bool is_integer(Type *type);
bool is_same_type(Type *a, Type *b);
bool is_compatible(Type *a, Tree *b);
