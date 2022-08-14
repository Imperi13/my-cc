
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "error.h"
#include "parse.h"
#include "type.h"

#ifndef __STDC__

void *calloc();
char *strncpy();

#endif

Type *gettype_decl_spec(DeclSpec *decl_spec, Analyze *state) {
  if (decl_spec->has_int) {
    return type_int;
  } else if (decl_spec->has_char) {
    return type_char;
  } else if (decl_spec->has_void) {
    return type_void;
  } else if (decl_spec->st_def) {
    return newtype_struct(decl_spec->st_def);
  } else if (decl_spec->en_def) {
    return type_int;
  } else if (decl_spec->def_name) {
    return find_typedef(state, decl_spec->def_name, decl_spec->def_len)->type;
  } else
    error("empty type");
  return 0;
  // return NULL;
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
  case ARRAY_DECLARATOR: {
    ArrayDeclarator *cur = declarator->arr_decl;
    while (cur) {
      Type *ty = calloc(1, sizeof(Type));
      ty->kind = ARRAY;
      ty->arr_size = eval_constexpr(cur->size);
      ty->ptr_to = base_type;
      base_type = ty;

      cur = cur->next;
    }
  } break;
  case NONE:
    break;
  default:
    error("invalid type_suffix");
  }

  if (declarator->nest) {
    base_type = gettype_declarator(declarator->nest, base_type);
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

Type *newtype_struct(StructDef *st_def) {
  Type *nt = calloc(1, sizeof(Type));
  nt->kind = STRUCT;
  nt->st_def = st_def;
  return nt;
}

int type_size(Type *type) {
  if (type->kind == INT)
    return 4;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == PTR)
    return 8;
  else if (type->kind == ARRAY)
    return type->arr_size * type_size(type->ptr_to);
  else if (type->kind == STRUCT)
    return type->st_def->size;
  else if (type->kind == FUNC)
    error("not defined type_size for FUNC");
  else
    not_implemented(__func__);
  return 0;
}

int type_alignment(Type *type) {
  if (type->kind == INT)
    return 4;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == PTR)
    return 8;
  else if (type->kind == ARRAY)
    return type_alignment(type->ptr_to);
  else if (type->kind == STRUCT)
    return type->st_def->alignment;
  else if (type->kind == FUNC)
    error("not defined type_alignment for FUNC");
  else
    not_implemented(__func__);
  return 0;
}

bool is_integer(Type *type) {
  if (type->kind == INT || type->kind == CHAR)
    return 1;
  // return true;
  return 0;
  // return false;
}

bool is_void_ptr(Type *type) {
  return type->kind == PTR && type->ptr_to->kind == VOID;
}

bool is_primitive_type(Type *a) {
  if (a->kind == VOID || a->kind == CHAR || a->kind == INT)
    return 1;
  // return true;
  else
    return 0;
  // return false;
}

bool is_same_type(Type *a, Type *b) {
  if (a->kind != b->kind)
    return 0;
  // return false;
  else if (is_primitive_type(a))
    return 1;
  // return true;
  else if (a->kind == PTR)
    return is_same_type(a->ptr_to, b->ptr_to);
  else if (a->kind == ARRAY)
    return a->arr_size == b->arr_size && is_same_type(a->ptr_to, b->ptr_to);
  else if (a->kind == STRUCT)
    return a->st_def == b->st_def;
  else if (a->kind == FUNC || b->kind == FUNC)
    error("type comp FUNC");
  return 0;
  // return false;
}

// Check if node b can be converted to type a
bool is_compatible(Type *a, Tree *b) {
  if (is_same_type(a, b->type))
    return 1;
  // return true;
  else if (is_integer(a) && is_integer(b->type))
    return 1;
  // return true;
  else if (a->kind == PTR && b->type->kind == PTR &&
           (is_void_ptr(a) || is_void_ptr(b->type)))
    return 1;
  // return true;

  // TODO start 暗黙の型変換はanalyzeに載せたい
  else if (is_void_ptr(a) && b->type->kind == FUNC)
    return 1;
  // return true;
  // TODO end

  else if (a->kind == PTR && is_constexpr(b) && eval_constexpr(b) == 0)
    return 1;
  // return true;

  return 0;
  // return false;
}
