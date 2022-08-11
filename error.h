#pragma once

#include "tokenize.h"

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

void not_implemented(const char *msg);
void not_implemented_at(char *loc);
