
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
  if(declarator->nest)
    return getargs_declarator(declarator->nest);

  if(declarator->type_suffix_kind != FUNC_DECLARATOR)
    error("not function");

  return declarator->args;
}
