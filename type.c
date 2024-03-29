
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "constexpr.h"
#include "error.h"
#include "parse.h"
#include "str_dict.h"
#include "type.h"
#include "vector.h"

Type type_void = {.kind = VOID};
Type type_bool = {.kind = BOOL};

Type type_longlong = {.kind = LONGLONG};
Type type_ulonglong = {.kind = LONGLONG, .is_unsigned = true};
Type type_long = {.kind = LONG};
Type type_ulong = {.kind = LONG, .is_unsigned = true};
Type type_int = {.kind = INT};
Type type_uint = {.kind = INT, .is_unsigned = true};
Type type_short = {.kind = SHORT};
Type type_ushort = {.kind = SHORT, .is_unsigned = true};
Type type_char = {.kind = CHAR};
Type type_uchar = {.kind = CHAR, .is_unsigned = true};

Type type_double = {.kind = DOUBLE};
Type type_float = {.kind = FLOAT};

void builtin_type_init(Analyze *state) {

  // struct __builtin_va_list
  StructDef *st_def = calloc(1, sizeof(StructDef));
  st_def->is_defined = true;
  st_def->size = 0x18;
  st_def->alignment = 0x8;

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

  Type *arr_type = calloc(1, sizeof(Type));
  arr_type->kind = ARRAY;
  arr_type->arr_size = 1;
  arr_type->ptr_to = newtype_struct(st_def);

  // typedef struct __builtin_va_list __builtin_va_list
  Typedef *new_def = calloc(1, sizeof(Typedef));
  new_def->name = "__builtin_va_list";
  new_def->type = arr_type;

  add_str_dict(state->glb_typedef_dict, new_def->name, new_def);
}

Type *gettype_decl_spec(DeclSpec *decl_spec) {
  if (decl_spec->type_spec_kind == TypeSpec_LONGLONG) {
    return &type_longlong;
  } else if (decl_spec->type_spec_kind == TypeSpec_ULONGLONG) {
    return &type_ulonglong;
  } else if (decl_spec->type_spec_kind == TypeSpec_LONG) {
    return &type_long;
  } else if (decl_spec->type_spec_kind == TypeSpec_ULONG) {
    return &type_ulong;
  } else if (decl_spec->type_spec_kind == TypeSpec_INT) {
    return &type_int;
  } else if (decl_spec->type_spec_kind == TypeSpec_UINT) {
    return &type_uint;
  } else if (decl_spec->type_spec_kind == TypeSpec_SHORT) {
    return &type_short;
  } else if (decl_spec->type_spec_kind == TypeSpec_USHORT) {
    return &type_ushort;
  } else if (decl_spec->type_spec_kind == TypeSpec_CHAR) {
    return &type_char;
  } else if (decl_spec->type_spec_kind == TypeSpec_UCHAR) {
    return &type_uchar;
  } else if (decl_spec->type_spec_kind == TypeSpec_VOID) {
    return &type_void;
  } else if (decl_spec->type_spec_kind == TypeSpec_BOOL) {
    return &type_bool;
  } else if (decl_spec->type_spec_kind == TypeSpec_DOUBLE) {
    return &type_double;
  } else if (decl_spec->type_spec_kind == TypeSpec_FLOAT) {
    return &type_float;
  } else if (decl_spec->st_def) {
    return newtype_struct(decl_spec->st_def);
  } else if (decl_spec->union_def) {
    return newtype_union(decl_spec->union_def);
  } else if (decl_spec->en_def) {
    return &type_int;
  } else if (decl_spec->defined_type) {
    return decl_spec->defined_type->type;
  } else
    error("empty type");
  return NULL;
}

Type *gettype_declarator(Declarator *declarator, Type *base_type) {

  if (declarator->nest)
    base_type = gettype_declarator(declarator->nest, base_type);

  for (Pointer *cur = declarator->pointer; cur; cur = cur->nest)
    base_type = newtype_ptr(base_type);

  switch (declarator->type_suffix_kind) {
  case FUNC_DECLARATOR: {
    Type *ty = calloc(1, sizeof(Type));
    ty->kind = FUNC;
    ty->return_type = base_type;

    // arg type
    ty->has_arg = declarator->has_arg_type;
    ty->has_variable_arg = declarator->has_variable_arg;

    ty->args_vector = new_vector();

    for (Tree *cur = declarator->args; cur; cur = cur->next) {
      Type *argtype = gettype_decl_spec(cur->decl_specs);
      if (cur->declarator)
        argtype = gettype_declarator(cur->declarator, argtype);

      if (argtype->kind == ARRAY)
        argtype->kind = PTR;

      push_back_vector(ty->args_vector, argtype);
    }

    base_type = ty;
  } break;
  case ARRAY_DECLARATOR: {
    ArrayDeclarator *cur = declarator->arr_decl;
    while (cur) {
      Type *ty = calloc(1, sizeof(Type));
      ty->kind = ARRAY;
      if (cur->is_null_size) {
        ty->is_null_size = true;
      } else {
        if (!is_constexpr_integer(cur->size_expr))
          error_token(cur->size_expr->error_token, "not constexpr");
        ty->arr_size = eval_constexpr_integer(cur->size_expr);
      }
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

  return base_type;
}

char *getname_declarator(Declarator *declarator) { return declarator->name; }

Tree *getargs_declarator(Declarator *declarator) {
  if (declarator->type_suffix_kind != FUNC_DECLARATOR)
    error("not function");

  return declarator->args;
}

ArrayDeclarator *get_arr_declarator(Declarator *declarator) {
  if (declarator->type_suffix_kind != ARRAY_DECLARATOR)
    error("not array");

  return declarator->arr_decl;
}

int integer_rank(Type *type) {
  if (!is_integer(type))
    error("not integer");

  if (type->kind == BOOL)
    return 0;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == SHORT)
    return 2;
  else if (type->kind == INT)
    return 3;
  else if (type->kind == LONG)
    return 4;
  else if (type->kind == LONGLONG)
    return 5;

  error("unreachable");
}

// bool,(u)char,(u)short convert to int
Type *get_integer_promoted_type(Type *integer_type) {
  assert(is_arithmetic(integer_type), "not integer type");

  if (integer_rank(integer_type) < integer_rank(&type_int))
    return &type_int;

  return integer_type;
}

Type *get_arithmetic_converted_type(Type *lhs_type, Type *rhs_type) {
  assert(is_arithmetic(lhs_type) && is_arithmetic(rhs_type),
         "not arithmetic type");

  if (is_same_type(lhs_type, rhs_type))
    return lhs_type;

  if (is_floating_point(lhs_type) || is_floating_point(rhs_type)) {
    if (lhs_type->kind == DOUBLE || rhs_type->kind == DOUBLE)
      return &type_double;
    else if (lhs_type->kind == FLOAT || rhs_type->kind == FLOAT)
      return &type_float;
    else
      error("not floating type");
  }

  if (lhs_type->is_unsigned == rhs_type->is_unsigned) {
    if (integer_rank(lhs_type) >= integer_rank(rhs_type))
      return lhs_type;
    else
      return rhs_type;
  }

  // one is signed, the other unsigned
  // swap so that lhs_type is signed
  if (lhs_type->is_unsigned) {
    Type *tmp = rhs_type;
    rhs_type = lhs_type;
    lhs_type = tmp;
  }

  if (integer_rank(lhs_type) <= integer_rank(rhs_type))
    return rhs_type;

  if (type_size(lhs_type) > type_size(rhs_type))
    return lhs_type;

  Type *result_type = calloc(1, sizeof(Type));
  result_type->kind = lhs_type->kind;
  result_type->is_unsigned = true;

  return result_type;
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
  if (type->kind == LONG || type->kind == LONGLONG)
    return 8;
  else if (type->kind == INT)
    return 4;
  else if (type->kind == SHORT)
    return 2;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == BOOL)
    return 1;
  else if (type->kind == PTR)
    return 8;
  else if (type->kind == FLOAT)
    return 4;
  else if (type->kind == DOUBLE)
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
  if (type->kind == LONG || type->kind == LONGLONG)
    return 8;
  else if (type->kind == INT)
    return 4;
  else if (type->kind == SHORT)
    return 2;
  else if (type->kind == CHAR)
    return 1;
  else if (type->kind == BOOL)
    return 1;
  else if (type->kind == PTR)
    return 8;
  else if (type->kind == FLOAT)
    return 4;
  else if (type->kind == DOUBLE)
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

bool is_arithmetic(Type *type) {
  return is_integer(type) || is_floating_point(type); // TODO check float
}

bool is_integer(Type *type) {
  if (type->kind == LONGLONG || type->kind == LONG || type->kind == INT ||
      type->kind == SHORT || type->kind == CHAR || type->kind == BOOL)
    return true;
  return false;
}

bool is_floating_point(Type *type) {
  if (type->kind == FLOAT || type->kind == DOUBLE)
    return true;
  return false;
}

bool is_scalar(Type *type) {
  if (is_integer(type) || type->kind == PTR)
    return true;
  else
    return false;
}

bool is_void_ptr(Type *type) {
  return type->kind == PTR && type->ptr_to->kind == VOID;
}

bool is_primitive_type(Type *type) {
  if (is_integer(type) || is_floating_point(type) || type->kind == VOID)
    return true;
  else
    return false;
}

bool is_same_type(Type *a, Type *b) {
  if (a->kind != b->kind)
    return false;
  else if (is_integer(a))
    return a->is_unsigned == b->is_unsigned;
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
  else if (a->kind == BOOL && is_scalar(b->type))
    return true;
  else if (is_arithmetic(a) && is_arithmetic(b->type))
    return true;
  else if (a->kind == PTR && b->type->kind == PTR &&
           (is_void_ptr(a) || is_void_ptr(b->type)))
    return true;
  else if (a->kind == PTR && is_constexpr_zero(b))
    return true;

  return false;
}
