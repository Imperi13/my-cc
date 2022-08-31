#pragma once

#include <stdbool.h>

#include "parse.h"

bool is_constexpr_integer(Tree *expr);
bool is_constexpr_zero(Tree *expr);

int eval_constexpr_integer(Tree *expr);
