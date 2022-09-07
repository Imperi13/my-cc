#pragma once

#include "tokenize.h"

_Noreturn void error(char *fmt, ...);
_Noreturn void error_token(Token *tok, char *fmt, ...);

_Noreturn void not_implemented(const char *msg);
_Noreturn void not_implemented_token(Token *tok);

void warn(char *fmt, ...);
void warn_token(Token *tok, char *fmt, ...);

