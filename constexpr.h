#pragma once

#include <stdbool.h>
#include <stdio.h>

#include "parse.h"
#include "type.h"

void codegen_global_initialize(FILE *codegen_output, Type *obj_type,
                               Tree *expr);

bool is_constexpr_integer(Tree *expr);
bool is_constexpr_zero(Tree *expr);

int eval_constexpr_integer(Tree *expr);
