
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

void codegen_global_initialize(FILE *codegen_output, Type *obj_type,
                               Tree *expr) {
  not_implemented(__func__);
}

bool is_constexpr(Tree *expr) { return false; }

bool is_constexpr_integer(Tree *expr) { return false; }

bool is_constexpr_zero(Tree *expr) { return false; }

int eval_constexpr_integer(Tree *expr) { return 0; }
