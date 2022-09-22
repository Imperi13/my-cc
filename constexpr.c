
#include <stdbool.h>
#include <stdio.h>

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

/*
 * ConstExpr{
 *   ConstExprType {INT,PTR}
 *   LabelName;
 *   Sign
 *   Interger
 * }
 */

void codegen_global_initialize(FILE *codegen_output, Type *obj_type,
                               Tree *expr) {
  if (is_integer(obj_type)) {
    if (type_size(obj_type) == 1)
      fprintf(codegen_output, "  .byte %d\n", eval_constexpr_integer(expr));
    else if (type_size(obj_type) == 4)
      fprintf(codegen_output, "  .long %d\n", eval_constexpr_integer(expr));
    else if (type_size(obj_type) == 8)
      fprintf(codegen_output, "  .quad %d\n", eval_constexpr_integer(expr));
  } else if (obj_type->kind == PTR) {
    // only impl NULL and STR
    if (expr->kind == STR) {
      fprintf(codegen_output, "  .quad .LC%d\n", expr->str_literal->id);
    } else if (expr->kind == CAST) {
      fprintf(codegen_output, "  .quad %d\n",
              eval_constexpr_integer(expr->lhs));
    } else
      not_implemented(__func__);
  } else if (obj_type->kind == ARRAY && obj_type->ptr_to->kind == CHAR &&
             expr->kind == STR) {
    // TODO str_literal initialize
    not_implemented(__func__);
  } else if (obj_type->kind == ARRAY) {
    int cnt = 0;
    for (InitializeList *cur = expr->init_list; cur; cur = cur->next) {
      codegen_global_initialize(codegen_output, obj_type->ptr_to,
                                cur->init_val);
      cnt++;
    }

    if (cnt > obj_type->arr_size)
      error("excess elements");

    if (cnt < obj_type->arr_size)
      fprintf(codegen_output, "  .zero %d\n",
              type_size(obj_type->ptr_to) * (obj_type->arr_size - cnt));

  } else if (obj_type->kind == STRUCT) {
    int write_bytes = 0;
    Member *mem_cur = obj_type->st_def->members;
    for (InitializeList *cur = expr->init_list; cur; cur = cur->next) {
      if (write_bytes < mem_cur->offset) {
        fprintf(codegen_output, "  .zero %d\n", mem_cur->offset - write_bytes);
        write_bytes = mem_cur->offset;
      }

      codegen_global_initialize(codegen_output, mem_cur->type, cur->init_val);
      write_bytes += type_size(mem_cur->type);

      mem_cur = mem_cur->next;
    }

    if (write_bytes < type_size(obj_type))
      fprintf(codegen_output, "  .zero %d\n",
              type_size(obj_type) - write_bytes);

  } else if (obj_type->kind == UNION) {
    not_implemented(__func__);
  } else
    error("invalid global_initialize");
}

bool is_constexpr(Tree *expr) {
  if (expr->kind == INITIALIZE_LIST) {
    for (InitializeList *cur = expr->init_list; cur; cur = cur->next)
      if (!is_constexpr(cur->init_val))
        return false;
    return true;
  } else if (expr->kind == CONDITIONAL)
    return is_constexpr_integer(expr->cond) &&
           (!is_constexpr_zero(expr->cond) ? is_constexpr(expr->lhs)
                                           : is_constexpr(expr->rhs));
  else if (expr->kind == LOGICAL_OR)
    return !is_constexpr(expr->lhs) || is_constexpr(expr->rhs);
  else if (expr->kind == LOGICAL_AND)
    return is_constexpr(expr->lhs) || is_constexpr(expr->rhs);
  else if (expr->kind == BIT_AND || expr->kind == BIT_XOR ||
           expr->kind == BIT_OR || expr->kind == EQUAL ||
           expr->kind == NOT_EQUAL || expr->kind == SMALLER ||
           expr->kind == SMALLER_EQUAL || expr->kind == GREATER ||
           expr->kind == GREATER_EQUAL || expr->kind == LSHIFT ||
           expr->kind == RSHIFT || expr->kind == ADD || expr->kind == SUB ||
           expr->kind == MUL || expr->kind == DIV || expr->kind == MOD)
    return is_constexpr(expr->lhs) && is_constexpr(expr->rhs);
  else if (expr->kind == CAST)
    return is_constexpr(expr->lhs);
  else if (expr->kind == PLUS || expr->kind == MINUS || expr->kind == BIT_NOT)
    return is_constexpr(expr->lhs);
  else if (expr->kind == LOGICAL_NOT)
    return is_constexpr(expr->lhs);
  else if (expr->kind == NUM || expr->kind == STR)
    return true;
  else
    return false;
}

bool is_constexpr_integer(Tree *expr) {
  if (expr->kind == CONDITIONAL)
    return is_constexpr_integer(expr->cond) &&
           (!is_constexpr_zero(expr->cond) ? is_constexpr_integer(expr->lhs)
                                           : is_constexpr_integer(expr->rhs));
  else if (expr->kind == LOGICAL_OR)
    return !is_constexpr_zero(expr->lhs) || is_constexpr_integer(expr->rhs);
  else if (expr->kind == LOGICAL_AND)
    return is_constexpr_zero(expr->lhs) || is_constexpr_integer(expr->rhs);
  else if (expr->kind == BIT_AND || expr->kind == BIT_XOR ||
           expr->kind == BIT_OR || expr->kind == EQUAL ||
           expr->kind == NOT_EQUAL || expr->kind == SMALLER ||
           expr->kind == SMALLER_EQUAL || expr->kind == GREATER ||
           expr->kind == GREATER_EQUAL || expr->kind == LSHIFT ||
           expr->kind == RSHIFT || expr->kind == ADD || expr->kind == SUB ||
           expr->kind == MUL || expr->kind == DIV || expr->kind == MOD)
    return is_constexpr_integer(expr->lhs) && is_constexpr_integer(expr->rhs);
  else if (expr->kind == CAST)
    return is_integer(expr->type) && is_constexpr_integer(expr->lhs);
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
  if (expr->kind == CONDITIONAL)
    return !is_constexpr_zero(expr->cond) ? eval_constexpr_integer(expr->lhs)
                                          : eval_constexpr_integer(expr->rhs);
  else if (expr->kind == LOGICAL_OR)
    return !is_constexpr_zero(expr->lhs) ? 1 : !is_constexpr_zero(expr->rhs);
  else if (expr->kind == LOGICAL_AND)
    return is_constexpr_zero(expr->lhs) ? 0 : !is_constexpr_zero(expr->rhs);
  else if (expr->kind == BIT_AND)
    return eval_constexpr_integer(expr->lhs) &
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == BIT_XOR)
    return eval_constexpr_integer(expr->lhs) ^
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == BIT_OR)
    return eval_constexpr_integer(expr->lhs) |
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == EQUAL)
    return eval_constexpr_integer(expr->lhs) ==
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == NOT_EQUAL)
    return eval_constexpr_integer(expr->lhs) !=
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == SMALLER)
    return eval_constexpr_integer(expr->lhs) <
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == SMALLER_EQUAL)
    return eval_constexpr_integer(expr->lhs) <=
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == GREATER)
    return eval_constexpr_integer(expr->lhs) >
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == GREATER_EQUAL)
    return eval_constexpr_integer(expr->lhs) >=
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == LSHIFT)
    return eval_constexpr_integer(expr->lhs)
           << eval_constexpr_integer(expr->rhs);
  else if (expr->kind == RSHIFT)
    return eval_constexpr_integer(expr->lhs) >>
           eval_constexpr_integer(expr->rhs);
  else if (expr->kind == ADD)
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
  else if (expr->kind == CAST)
    return eval_constexpr_integer(expr->lhs);
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
