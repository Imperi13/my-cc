#pragma once

typedef enum {
  VOID,
  INT,
  CHAR,
  PTR,
  STRUCT,
  FUNC,
  ARRAY,
} TypeKind;

typedef struct Type Type;

#include "parse.h"
#include "analyze.h"

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

  // for linked-list
  Type *next;
};

extern Type *type_void;
extern Type *type_int;
extern Type *type_char;

Type *gettype_decl_spec(DeclSpec *decl_spec);
Type *gettype_declarator(Declarator *declarator, Type *base_type);
char *getname_declarator(Declarator *declarator);
Tree *getargs_declarator(Declarator *declarator);

Type *newtype_ptr(Type *type);
Type *newtype_struct(StructDef *st_def);

int type_size(Type *type);
int type_alignment(Type *type);

bool is_integer(Type *type);
bool is_same_type(Type *a,Type *b);
bool is_compatible(Type *a,Type *b);
