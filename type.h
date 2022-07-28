#pragma once

typedef enum {
  INT,
  PTR,
  FUNC,
} TypeKind;

typedef struct Type Type;

#include "parse.h"

struct Type {
  TypeKind kind;

  // for FUNC
  Type *return_type;

  // for PTR
  Type *ptr_to;

  // for linked-list
  Type *next;
};

extern Type *type_int;

Type *gettype_decl_spec(DeclSpec *decl_spec);
Type *gettype_declarator(Declarator *declarator, Type *base_type);
char *getname_declarator(Declarator *declarator);
Tree *getargs_declarator(Declarator *declarator);

int type_size(Type *type);
int type_alignment(Type *type);
