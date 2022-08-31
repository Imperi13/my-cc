#pragma once

#include <stdbool.h>

#include "parse.h"

bool is_constexpr(Tree *expr);

int eval_constexpr(Tree *expr);
