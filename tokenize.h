#pragma once

#include <stdbool.h>
#include <stdio.h>

typedef enum TokenKind {
  TK_RESERVED,
  TK_IDENT,
  TK_NUM,
  TK_STR,
  TK_NEWLINE,
  TK_EOF,

  // for func-like macro
  TK_MACRO_ARG,

  // keyword
  TK_RETURN,
  TK_GOTO,
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
  TK_VOID,
  TK_LONG,
  TK_INT,
  TK_CHAR,
  TK_SHORT,
  TK_BOOL,
  TK_SIGNED,
  TK_UNSIGNED,
  TK_FLOAT,
  TK_DOUBLE,
  TK_CONST,
  TK_RESTRICT,
  TK_VOLATILE,
  TK_EXTERN,
  TK_STATIC,
  TK_TYPEDEF,
  TK_INLINE,
  TK_NORETURN,
} TokenKind;

typedef struct Token Token;
typedef struct StrLiteral StrLiteral;

struct Token {
  TokenKind kind;
  Token *next;
  char *str;
  size_t len;

  // for preprocess & error-handling
  char *filepath;
  char *file_buf;
  bool is_recursived;

  // for func-like macro
  int nth_arg;
  bool is_va_args;

  // for TK_NUM
  unsigned long long val;
  bool is_unsigned;
  bool is_long;
  bool is_longlong;

  // floating constant
  bool is_floatint_constant;
  double floating_val;

  // for TK_IDENT
  char *ident_str;

  // for str-literal
  StrLiteral *str_literal;
};

struct StrLiteral {
  char *str;
  int len;
  int id;
};

extern const char *variable_letters;

bool equal(Token *token, char *op);
bool equal_kind(Token *token, TokenKind kind);
bool consume(Token **rest, Token *token, char *op);
Token *consume_kind(Token **rest, Token *token, TokenKind kind);
void expect(Token **rest, Token *token, char *op);
Token *expect_kind(Token **rest, Token *token, TokenKind kind);
int expect_number(Token **rest, Token *token);
void expect_ident(Token **rest, Token *token, char *name);
char *getname_ident(Token **rest, Token *tok);
bool cmp_ident(Token *tok, const char *name);
bool at_eof(Token *token);

bool is_same_token(Token *a, Token *b);

bool is_alnum(char c);

Token *new_eof_token(void);
Token *copy_and_consume(Token **rest, Token *tok);
Token *copy_token_seq(Token *tok);

Token *tokenize(char *p, char *filepath);

void print_token_seq(FILE *output_stream, Token *tok);
void debug_token(Token *token);
