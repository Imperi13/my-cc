
#include <stdbool.h>

#include "constexpr.h"
#include "error.h"
#include "parse.h"

bool is_constexpr(Tree *expr) {
  if (expr->kind == NUM)
    return true;
  else
    return false;
}

int eval_constexpr(Tree *expr) {
  if (expr->kind == NUM)
    return expr->num;
  else
    not_implemented(__func__);
  return 0;
}
