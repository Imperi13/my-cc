#pragma once

#include <stdio.h>

#include "parse.h"

#ifndef __STDC__

#include "selfhost_util.h"

#endif

extern FILE *codegen_output;

void codegen_translation_unit(Tree *head);
