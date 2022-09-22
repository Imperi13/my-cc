
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
  not_implemented(__func__);
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
  case NUM:
    return true;
  default:
    return false;
  }
}

// TODO pass by value
ConstValue *eval_constexpr(Tree *expr) {
  if (!is_constexpr(expr))
    error("not constexpr");

  ConstValue *ret = calloc(1, sizeof(ConstValue));

  switch (expr->kind) {
  case NUM: {
    ret->value_int = expr->num;
    return ret;
  }
  default:
    error("invalid ast kind");
  }
}
