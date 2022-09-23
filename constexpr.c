
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
  } else if (obj_type->kind == PTR) {
    ConstValue *val = eval_constexpr(expr);
    if (val->label_name) {
      fprintf(codegen_output, "  .quad %s + %ld\n", val->label_name,
              val->value_int);
    } else {
      if (val->value_int != 0)
        error("not zero constexpr integer cannot be ptr");
      fprintf(codegen_output, "  .quad 0\n");
    }
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
    // binary op

  case BIT_AND:
  case BIT_XOR:
  case BIT_OR:
  case LSHIFT:
  case RSHIFT:
  case ADD:
  case SUB:
  case MUL:
  case DIV:
  case MOD: {
    return is_constexpr(expr->lhs) && is_constexpr(expr->rhs);
  } break;

  case CAST: {
    return is_constexpr(expr->lhs);
  } break;
  case NUM: {
    return true;
  } break;
  case STR: {
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
  case BIT_OR: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int |= rhs->value_int;
    return lhs;
  } break;
  case BIT_XOR: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int ^= rhs->value_int;
    return lhs;
  } break;
  case BIT_AND: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int &= rhs->value_int;
    return lhs;
  } break;
  case LSHIFT: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int <<= rhs->value_int;
    return lhs;
  } break;
  case RSHIFT: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int >>= rhs->value_int;
    return lhs;
  } break;
  case ADD: {
    if (is_integer(expr->lhs->type) && is_integer(expr->rhs->type)) {
      ConstValue *lhs = eval_constexpr(expr->lhs);
      ConstValue *rhs = eval_constexpr(expr->rhs);
      lhs->value_int += rhs->value_int;
      return lhs;
    } else
      not_implemented(__func__);
  } break;
  case SUB: {
    if (is_integer(expr->lhs->type) && is_integer(expr->rhs->type)) {
      ConstValue *lhs = eval_constexpr(expr->lhs);
      ConstValue *rhs = eval_constexpr(expr->rhs);
      lhs->value_int -= rhs->value_int;
      return lhs;
    } else
      not_implemented(__func__);
  } break;
  case MUL: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int *= rhs->value_int;
    return lhs;
  } break;
  case DIV: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int /= rhs->value_int;
    return lhs;
  } break;
  case MOD: {
    ConstValue *lhs = eval_constexpr(expr->lhs);
    ConstValue *rhs = eval_constexpr(expr->rhs);
    lhs->value_int %= rhs->value_int;
    return lhs;
  } break;
  case CAST: {
    if (expr->type->kind == BOOL && is_scalar(expr->lhs->type)) {
      ConstValue *ret = calloc(1, sizeof(ConstValue));
      if (is_constexpr_zero(expr->lhs))
        ret->value_int = 0;
      else
        ret->value_int = 1;
      return ret;
    } else {
      return eval_constexpr(expr->lhs);
    }
  } break;
  case NUM: {
    ConstValue *ret = calloc(1, sizeof(ConstValue));
    ret->value_int = expr->num;
    return ret;
  } break;
  case STR: {
    ConstValue *ret = calloc(1, sizeof(ConstValue));
    ret->label_name = calloc(36, sizeof(char));
    snprintf(ret->label_name, 36, ".LC%d", expr->str_literal->id);
    return ret;
  } break;
  default:
    error("invalid ast kind");
  }
}
