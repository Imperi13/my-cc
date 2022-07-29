
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "parse.h"
#include "type.h"

Type *type_int = &(Type){.kind = INT};

Type *gettype_decl_spec(DeclSpec *decl_spec) {
  if (decl_spec->has_int) {
    return type_int;
  }
  error("empty type");
  return NULL;
}

Type *gettype_declarator(Declarator *declarator, Type *base_type) {
  Pointer *cur = declarator->pointer;
  while (cur) {
    base_type = newtype_ptr(base_type);
    cur = cur->nest;
  }

  switch (declarator->type_suffix_kind) {
  case FUNC_DECLARATOR: {
    Type *ty = calloc(1, sizeof(Type));
    ty->kind = FUNC;
    ty->return_type = base_type;
    base_type = ty;
  } break;
  case ARRAY_DECLARATOR:
    not_implemented();
    break;
  case NONE:
    break;
  default:
    error("invalid type_suffix");
  }

  if (declarator->nest) {
    not_implemented();
  }

  return base_type;
}

char *getname_declarator(Declarator *declarator) {
  if (declarator->nest)
    return getname_declarator(declarator->nest);

  char *ret = calloc(1, declarator->len + 1);
  strncpy(ret, declarator->name, declarator->len);

  return ret;
}

Tree *getargs_declarator(Declarator *declarator) {
  if (declarator->nest)
    return getargs_declarator(declarator->nest);

  if (declarator->type_suffix_kind != FUNC_DECLARATOR)
    error("not function");

  return declarator->args;
}

Type *newtype_ptr(Type *type) {
  Type *nt = calloc(1, sizeof(Type));
  nt->kind = PTR;
  nt->ptr_to = type;
  return nt;
}

int type_size(Type *type) {
  if (type->kind == INT)
    return 4;
  if (type->kind == PTR)
    return 8;
  if (type->kind == FUNC)
    error("not defined type_size for FUNC");
  return 0;
}

int type_alignment(Type *type) {
  if (type->kind == INT)
    return 4;
  if (type->kind == PTR)
    return 8;
  if (type->kind == FUNC)
    error("not defined type_alignment for FUNC");
  return 0;
}
