#pragma once

typedef enum {
  INT,
  CHAR,
  PTR,
  FUNC,
  ARRAY,
} TypeKind;

typedef struct Type Type;

#include "parse.h"

struct Type {
  TypeKind kind;

  // for FUNC
  Type *return_type;

  // for PTR,ARRAY
  Type *ptr_to;

  // for ARRAY
  int arr_size;

  // for linked-list
  Type *next;
};

extern Type *type_int;
extern Type *type_char;

Type *gettype_decl_spec(DeclSpec *decl_spec);
Type *gettype_declarator(Declarator *declarator, Type *base_type);
char *getname_declarator(Declarator *declarator);
Tree *getargs_declarator(Declarator *declarator);

Type *newtype_ptr(Type *type);

int type_size(Type *type);
int type_alignment(Type *type);

bool is_integer(Type *type);
