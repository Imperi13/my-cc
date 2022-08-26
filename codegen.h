#pragma once

#include <stdio.h>

#include "parse.h"

#ifndef __STDC__

#include "selfhost_util.h"

#endif

void codegen_translation_unit(FILE *codegen_output,Tree *head);
