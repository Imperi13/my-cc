#pragma once

#include "tokenize.h"

void error(char *fmt, ...);
void error_token(Token *tok, char *fmt, ...);

void not_implemented(const char *msg);
void not_implemented_token(Token *tok);

void warn(char *fmt, ...);
void warn_token(Token *tok, char *fmt, ...);

