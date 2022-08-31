
#include <stdbool.h>

#include "constexpr.h"
#include "error.h"
#include "parse.h"

bool is_constexpr_integer(Tree *expr) {
  if (expr->kind == ADD || expr->kind == SUB || expr->kind == MUL ||
      expr->kind == DIV || expr->kind == MOD)
    return is_constexpr_integer(expr->lhs) && is_constexpr_integer(expr->rhs);
  else if (expr->kind == PLUS || expr->kind == MINUS || expr->kind == BIT_NOT)
    return is_constexpr_integer(expr->lhs);
  else if (expr->kind == LOGICAL_NOT)
    return is_constexpr_integer(expr->lhs);
  else if (expr->kind == NUM)
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
  if (expr->kind == ADD)
    return eval_constexpr_integer(expr->lhs) +
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == SUB)
    return eval_constexpr_integer(expr->lhs) -
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == MUL)
    return eval_constexpr_integer(expr->lhs) *
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == DIV)
    return eval_constexpr_integer(expr->lhs) /
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == MOD)
    return eval_constexpr_integer(expr->lhs) %
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == PLUS)
    return eval_constexpr_integer(expr->lhs);
  else if (expr->kind == MINUS)
    return -eval_constexpr_integer(expr->lhs);
  else if (expr->kind == LOGICAL_NOT)
    return !eval_constexpr_integer(expr->lhs);
  else if (expr->kind == BIT_NOT)
    return ~eval_constexpr_integer(expr->lhs);
  else if (expr->kind == NUM)
    return expr->num;
  else
    not_implemented(__func__);
  return 0;
}
