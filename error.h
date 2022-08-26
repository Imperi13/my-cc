#pragma once

#include "tokenize.h"

#ifdef __STDC__

void error(char *fmt, ...);
void error_token(Token *tok, char *fmt, ...);

void not_implemented(const char *msg);
void not_implemented_token(Token *tok);

void warn(char *fmt, ...);
void warn_token(Token *tok, char *fmt, ...);

#endif

#ifndef __STDC__

void error();
void error_token();

void not_implemented();
void not_implemented_token();

void warn();
void warn_token();

#endif
