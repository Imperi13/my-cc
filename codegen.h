#pragma once

#include <stdio.h>

#include "parse.h"
#include "vector.h"

extern Vector *str_literal_vector;

void codegen_translation_unit(FILE *codegen_output, Tree *head);
