
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "constexpr.h"
#include "error.h"
#include "parse.h"
#include "str_dict.h"
#include "type.h"

Type type_void = {VOID};
Type type_long = {LONG};
Type type_int = {INT};
Type type_char = {CHAR};
Type type_bool = {BOOL};

void builtin_type_init(Analyze *state) {

  // struct __builtin_va_list
  StructDef *st_def = calloc(1, sizeof(StructDef));
  st_def->st_name = "__builtin_va_list";
  st_def->is_defined = true;
  st_def->size = 0x18;
  st_def->alignment = 0x8;

  add_str_dict(state->glb_struct_def_dict, st_def->st_name, st_def);

  st_def->members = calloc(1, sizeof(Member));
  Member *cur = st_def->members;

  cur->member_name = "gp_offset";
  cur->type = &type_int;
  cur->offset = 0x0;

  cur->next = calloc(1, sizeof(Member));
  cur = cur->next;

  cur->member_name = "fp_offset";
  cur->type = &type_int;
  cur->offset = 0x4;

  cur->next = calloc(1, sizeof(Member));
  cur = cur->next;

  cur->member_name = "overflow_arg_area";
  cur->type = newtype_ptr(&type_void);
  cur->offset = 0x8;

  cur->next = calloc(1, sizeof(Member));
  cur = cur->next;

  cur->member_name = "reg_save_area";
  cur->type = newtype_ptr(&type_void);
  cur->offset = 0x10;

  // typedef struct __builtin_va_list __builtin_va_list
  Typedef *new_def = calloc(1, sizeof(Typedef));
  new_def->name = "__builtin_va_list";
  new_def->type = newtype_struct(st_def);

  add_str_dict(state->glb_typedef_dict, new_def->name, new_def);
}

Type *gettype_decl_spec(DeclSpec *decl_spec, Analyze *state) {
  if (decl_spec->type_spec_kind == TypeSpec_LONG) {
    return &type_long;
  } else if (decl_spec->type_spec_kind == TypeSpec_INT) {
    return &type_int;
  } else if (decl_spec->type_spec_kind == TypeSpec_CHAR) {
    return &type_char;
  } else if (decl_spec->type_spec_kind == TypeSpec_VOID) {
    return &type_void;
  } else if (decl_spec->type_spec_kind == TypeSpec_BOOL) {
    return &type_bool;
  } else if (decl_spec->st_def) {
    return newtype_struct(decl_spec->st_def);
  } else if (decl_spec->union_def) {
    return newtype_union(decl_spec->union_def);
  } else if (decl_spec->en_def) {
    return &type_int;
  } else if (decl_spec->def_name) {
    return find_typedef(state, decl_spec->def_name)->type;
  } else
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
  case ARRAY_DECLARATOR: {
    ArrayDeclarator *cur = declarator->arr_decl;
    while (cur) {
      Type *ty = calloc(1, sizeof(Type));
      ty->kind = ARRAY;
      ty->arr_size = eval_constexpr_integer(cur->size);
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

  return declarator->name;
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

Type *newtype_union(UnionDef *union_def) {
  Type *nt = calloc(1, sizeof(Type));
  nt->kind = UNION;
  nt->union_def = union_def;
  return nt;
}

int type_size(Type *type) {
  if (type->kind == LONG)
    return 8;
  else if (type->kind == INT)
    return 4;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == BOOL)
    return 1;
  else if (type->kind == PTR)
    return 8;
  else if (type->kind == ARRAY)
    return type->arr_size * type_size(type->ptr_to);
  else if (type->kind == STRUCT)
    return type->st_def->size;
  else if (type->kind == UNION)
    return type->union_def->size;
  else if (type->kind == FUNC)
    error("not defined type_size for FUNC");
  else
    not_implemented(__func__);
  return 0;
}

int type_alignment(Type *type) {
  if (type->kind == LONG)
    return 8;
  else if (type->kind == INT)
    return 4;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == BOOL)
    return 1;
  else if (type->kind == PTR)
    return 8;
  else if (type->kind == ARRAY)
    return type_alignment(type->ptr_to);
  else if (type->kind == STRUCT)
    return type->st_def->alignment;
  else if (type->kind == UNION)
    return type->union_def->alignment;
  else if (type->kind == FUNC)
    error("not defined type_alignment for FUNC");
  else
    not_implemented(__func__);
  return 0;
}

bool is_integer(Type *type) {
  if (type->kind == LONG || type->kind == INT || type->kind == CHAR)
    return true;
  return false;
}

bool is_void_ptr(Type *type) {
  return type->kind == PTR && type->ptr_to->kind == VOID;
}

bool is_primitive_type(Type *a) {
  if (a->kind == VOID || a->kind == CHAR || a->kind == LONG || a->kind == INT ||
      a->kind == BOOL)
    return true;
  else
    return false;
}

bool is_same_type(Type *a, Type *b) {
  if (a->kind != b->kind)
    return false;
  else if (is_primitive_type(a))
    return true;
  else if (a->kind == PTR)
    return is_same_type(a->ptr_to, b->ptr_to);
  else if (a->kind == ARRAY)
    return a->arr_size == b->arr_size && is_same_type(a->ptr_to, b->ptr_to);
  else if (a->kind == STRUCT)
    return a->st_def == b->st_def;
  else if (a->kind == UNION)
    return a->union_def == b->union_def;
  else if (a->kind == FUNC)
    return is_same_type(a->return_type, b->return_type);
  return false;
}

// Check if node b can be converted to type a
bool is_compatible(Type *a, Tree *b) {
  if (is_same_type(a, b->type))
    return true;
  else if (is_integer(a) && is_integer(b->type))
    return true;
  else if (a->kind == BOOL && is_integer(b->type))
    return true;
  else if (a->kind == PTR && b->type->kind == PTR &&
           (is_void_ptr(a) || is_void_ptr(b->type)))
    return true;

  // TODO start 暗黙の型変換はanalyzeに載せたい
  else if (is_void_ptr(a) && b->type->kind == FUNC)
    return true;
  // TODO end

  else if (a->kind == PTR && is_constexpr_zero(b))
    return true;

  return false;
}
