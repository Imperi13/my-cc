
#include <stdbool.h>

#include "constexpr.h"
#include "error.h"
#include "parse.h"

bool is_constexpr_integer(Tree *expr) {
  if (expr->kind == NUM)
    return true;
  else
    return false;
}

bool is_constexpr_zero(Tree *expr) {
  if (!is_constexpr_integer(expr))
    return false;
  return eval_constexpr_integer(expr) == 0;
}

int eval_constexpr_integer(Tree *expr) {
  if (expr->kind == NUM)
    return expr->num;
  else
    not_implemented(__func__);
  return 0;
}
