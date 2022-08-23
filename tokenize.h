#pragma once

#include <stdbool.h>

#ifndef __STDC__

#include "selfhost_util.h"

#endif

typedef enum TokenKind {
  TK_RESERVED,
  TK_RETURN,
  TK_SIZEOF,
  TK_ALIGNOF,
  TK_STRUCT,
  TK_UNION,
  TK_ENUM,
  TK_IF,
  TK_ELSE,
  TK_SWITCH,
  TK_DEFAULT,
  TK_CASE,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_BREAK,
  TK_CONTINUE,
  TK_IDENT,
  TK_VOID,
  TK_LONG,
  TK_INT,
  TK_CHAR,
  TK_BOOL,
  TK_CONST,
  TK_EXTERN,
  TK_STATIC,
  TK_TYPEDEF,
  TK_NUM,
  TK_STR,
  TK_NEWLINE,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
typedef struct StrLiteral StrLiteral;

struct Token {
  TokenKind kind;
  Token *next;
  int val;
  char *str;
  int len;

  char *filepath;

  // for TK_IDENT
  char *ident_str;

  // for str-literal
  StrLiteral *str_literal;
};

struct StrLiteral {
  char *str;
  int len;
  int id;

  // for linked-list
  StrLiteral *next;
};

extern const char *variable_letters;

extern StrLiteral *str_literals;

bool equal(Token *token, char *op);
bool equal_kind(Token *token, TokenKind kind);
bool consume(Token **rest, Token *token, char *op);
Token *consume_kind(Token **rest, Token *token, TokenKind kind);
void expect(Token **rest, Token *token, char *op);
Token *expect_kind(Token **rest, Token *token, TokenKind kind);
int expect_number(Token **rest, Token *token);
char *getname_ident(Token **rest,Token *tok);
bool cmp_ident(Token *tok, const char *name);
bool at_eof(Token *token);

bool is_alnum(char c);

Token *tokenize(char *p, char *filepath);

void debug_token(Token *token);
