
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "constexpr.h"
#include "error.h"
#include "parse.h"
#include "type.h"

/*
 * const expression
 * - constexpr interger
 * - constexpr pointer
 *   - & global variable
 *   - (void *) constexpr 0
 */

// current ConstValue impl cannot handle the following code
// ex)
// unsigned long long a = 0xffff,ffff,ffff,ffff;
// long long b = -0x8000,0000,0000,0000;
//
// TODO Implement "strict" compile-time constants

typedef struct ConstValue ConstValue;
struct ConstValue {
  char *label_name;
  long value_int;
};

static ConstValue *eval_constexpr(Tree *expr);

void codegen_global_initialize(FILE *codegen_output, Type *obj_type,
                               Tree *expr) {
  if (is_integer(obj_type)) {
    if (type_size(obj_type) == 1)
      fprintf(codegen_output, "  .byte %ld\n", eval_constexpr_integer(expr));
    else if (type_size(obj_type) == 2)
      fprintf(codegen_output, "  .short %ld\n", eval_constexpr_integer(expr));
    else if (type_size(obj_type) == 4)
      fprintf(codegen_output, "  .long %ld\n", eval_constexpr_integer(expr));
    else if (type_size(obj_type) == 8)
      fprintf(codegen_output, "  .quad %ld\n", eval_constexpr_integer(expr));
    else
      error("invalid type_size");
  } else {
    not_implemented(__func__);
  }
}

bool is_constexpr_integer(Tree *expr) {
  return is_constexpr(expr) && is_integer(expr->type);
}

// check if expr is 0, nullptr
bool is_constexpr_zero(Tree *expr) {
  if (!is_constexpr(expr))
    return false;
  ConstValue *val = eval_constexpr(expr);
  return !val->label_name && val->value_int == 0;
}

long eval_constexpr_integer(Tree *expr) {
  if (!is_constexpr_integer(expr))
    error("not constexpr integer");
  return eval_constexpr(expr)->value_int;
}

bool is_constexpr(Tree *expr) {
  switch (expr->kind) {
  case CAST: {
    if (is_integer(expr->type) && is_integer(expr->lhs->type))
      return true;
    else
      not_implemented(__func__);
  } break;
  case NUM: {
    return true;
  } break;
  default:
    return false;
  }
}

// TODO pass by value
ConstValue *eval_constexpr(Tree *expr) {
  if (!is_constexpr(expr))
    error("not constexpr");

  switch (expr->kind) {
  case CAST: {
    if (is_integer(expr->type) && is_integer(expr->lhs->type)) {
      return eval_constexpr(expr->lhs);
    } else
      not_implemented(__func__);
  } break;
  case NUM: {
    ConstValue *ret = calloc(1, sizeof(ConstValue));
    ret->value_int = expr->num;
    return ret;
  } break;
  default:
    error("invalid ast kind");
  }
}
