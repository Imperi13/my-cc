#pragma once

#ifdef __STDC__

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

void not_implemented(const char *msg);
void not_implemented_at(char *loc);

void warn(char *fmt, ...);
void warn_at(char *loc, char *fmt, ...);

#endif

#ifndef __STDC__

void error();
void error_at();

void not_implemented();
void not_implemented_at();

void warn();
void warn_at();

#endif
