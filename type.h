#pragma once

typedef enum TypeKind {
  VOID,
  LONG,
  INT,
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

struct Type {
  TypeKind kind;

  // for FUNC
  Type *return_type;

  // for PTR,ARRAY
  Type *ptr_to;

  // for ARRAY
  int arr_size;

  // for STRUCT
  StructDef *st_def;

  // for UNION
  UnionDef *union_def;

  // for linked-list
  Type *next;
};

extern Type type_void;
extern Type type_long;
extern Type type_int;
extern Type type_char;
extern Type type_bool;

void builtin_type_init(Analyze *state);

Type *gettype_decl_spec(DeclSpec *decl_spec, Analyze *state);
Type *gettype_declarator(Declarator *declarator, Type *base_type,
                         Analyze *state);
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
