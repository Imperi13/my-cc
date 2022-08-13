#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "tokenize.h"

bool equal(Token *token, char *op) {
  if (token->kind != TK_RESERVED || strlen(op) != token->len ||
      memcmp(token->str, op, token->len))
    return false;
  return true;
}

bool equal_kind(Token *token, TokenKind kind) { return token->kind == kind; }

bool consume(Token **rest, Token *token, char *op) {
  if (token->kind != TK_RESERVED || strlen(op) != token->len ||
      memcmp(token->str, op, token->len)) {
    *rest = token;
    return false;
  }
  *rest = token->next;
  return true;
}

Token *consume_kind(Token **rest, Token *token, TokenKind kind) {
  if (token->kind != kind) {
    *rest = token;
    return NULL;
  }
  *rest = token->next;
  return token;
}

void expect(Token **rest, Token *token, char *op) {
  if (token->kind != TK_RESERVED || strlen(op) != token->len ||
      memcmp(token->str, op, token->len))
    error_at(token->str, "not '%c' op", op);
  *rest = token->next;
}

Token *expect_kind(Token **rest, Token *token, TokenKind kind) {
  if (token->kind != kind)
    error_at(token->str, "not expect TokenKind");
  *rest = token->next;
  return token;
}

int expect_number(Token **rest, Token *token) {
  if (token->kind != TK_NUM)
    error_at(token->str, "not number");
  int val = token->val;
  *rest = token->next;
  return val;
}

bool at_eof(Token *token) { return token->kind == TK_EOF; }

Token *new_token(TokenKind kind, Token *cur, char *str, int len) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
  cur->next = tok;
  return tok;
}

const char variable_letters[] =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

Token *dummy_token = &(Token){};

bool is_alnum(char c) {
  for (const char *p = variable_letters; *p != '\0'; p++)
    if (*p == c)
      return true;
  return false;
}

char *skip_space(char *str, char c) {
  while (*str == ' ')
    str++;

  if (*str == c)
    return str;
  return NULL;
}

int digit_base(char t, unsigned long base) {
  if (base == 16) {
    if ('0' <= t && t <= '9')
      return t - '0';
    if ('a' <= t && t <= 'f')
      return 10 + t - 'a';
    if ('A' <= t && t <= 'F')
      return 10 + t - 'A';
    return -1;
  }
  if (base == 10) {
    if ('0' <= t && t <= '9')
      return t - '0';
    return -1;
  }
  if (base == 8) {
    if ('0' <= t && t <= '7')
      return t - '0';
    return -1;
  }
  if (base == 2) {
    if ('0' <= t && t <= '1')
      return t - '0';
    return -1;
  }

  error("invalid num_base");
  return -1;
}

unsigned long num_literal(char *p, char **rest) {
  unsigned long base;
  char *start;

  if (*p != '0') {
    base = 10;
    start = p;
  } else if (p[1] == 'x' || p[1] == 'X') {
    base = 16;
    start = p + 2;
  } else if (p[1] == 'b' || p[1] == 'B') {
    base = 2;
    start = p + 2;
  } else {
    base = 8;
    start = p + 1;
  }

  unsigned long num = 0;
  while (digit_base(*start, base) >= 0) {
    num *= base;
    num += digit_base(*start, base);
    start++;
  }
  *rest = start;
  return num;
}

char consume_char(char **rest, char *p) {
  if (*p == '\\') {
    char ret;
    p++;
    if (*p == 'e') {
      ret = '\e';
    } else if (*p == 'n') {
      ret = '\n';
    } else if (*p == '0') {
      ret = '\0';
    } else {
      not_implemented(__func__);
    }
    p++;

    *rest = p;
    return ret;
  } else {
    char ret = *p;
    p++;

    *rest = p;
    return ret;
  }
}

StrLiteral *str_literals = NULL;

Token *tokenize(char *p) {
  Token head;
  head.next = NULL;
  Token *cur = &head;

  while (*p) {
    if (isblank(*p)) {
      p++;
      continue;
    }

    if (*p == '\n') {
      cur = new_token(TK_NEWLINE, cur, p, 1);
      p++;
      continue;
    }

    if (*p == '"') {
      char *tok_start = p;
      char *str_cur = tok_start + 1;
      int len = 0;
      while (*str_cur != '\"') {
        len++;
        consume_char(&str_cur, str_cur);
      }

      StrLiteral *push_literal = calloc(1, sizeof(StrLiteral));
      char *str = malloc(len + 1);
      push_literal->str = str;
      push_literal->len = len;

      str_cur = tok_start + 1;
      int i = 0;
      while (*str_cur != '\"') {
        str[i] = consume_char(&str_cur, str_cur);
        i++;
      }

      if (!str_literals) {
        push_literal->id = 0;
        str_literals = push_literal;
      } else {
        push_literal->id = str_literals->id + 1;
        push_literal->next = str_literals;
        str_literals = push_literal;
      }

      Token *tmp = new_token(TK_STR, cur, tok_start, str_cur - tok_start + 1);
      tmp->str_literal = push_literal;

      p = str_cur + 1;
      cur = tmp;
      continue;
    }

    if (strncmp(p, "//", 2) == 0) {
      p += 2;
      while (*p != '\n')
        p++;
      continue;
    }

    if (strncmp(p, "/*", 2) == 0) {
      char *q = strstr(p + 2, "*/");
      if (!q)
        error_at(p, "not found comment end */");
      p = q + 2;
      continue;
    }

    if (strncmp(p, "<<=", 3) == 0 || strncmp(p, ">>=", 3) == 0) {
      cur = new_token(TK_RESERVED, cur, p, 3);
      p += 3;
      continue;
    }

    if (strncmp(p, ">=", 2) == 0 || strncmp(p, "<=", 2) == 0 ||
        strncmp(p, "==", 2) == 0 || strncmp(p, "!=", 2) == 0 ||
        strncmp(p, "+=", 2) == 0 || strncmp(p, "-=", 2) == 0 ||
        strncmp(p, "*=", 2) == 0 || strncmp(p, "/=", 2) == 0 ||
        strncmp(p, "%=", 2) == 0 || strncmp(p, "&=", 2) == 0 ||
        strncmp(p, "|=", 2) == 0 || strncmp(p, "^=", 2) == 0 ||
        strncmp(p, "++", 2) == 0 || strncmp(p, "--", 2) == 0 ||
        strncmp(p, "<<", 2) == 0 || strncmp(p, ">>", 2) == 0 ||
        strncmp(p, "&&", 2) == 0 || strncmp(p, "||", 2) == 0 ||
        strncmp(p, "->", 2) == 0 || strncmp(p, "##", 2) == 0) {
      cur = new_token(TK_RESERVED, cur, p, 2);
      p += 2;
      continue;
    }

    if (strchr("+-*/|&!~^%:?.,;=(){}[]<>#", *p)) {
      cur = new_token(TK_RESERVED, cur, p, 1);
      p++;
      continue;
    }

    if (isdigit(*p)) {
      char *prev = p;
      cur = new_token(TK_NUM, cur, p, 1);
      cur->val = num_literal(p, &p);
      cur->len = p - prev;
      continue;
    }

    if (strncmp(p, "return", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_RETURN, cur, p, 6);
      p += 6;
      continue;
    }

    if (strncmp(p, "sizeof", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_SIZEOF, cur, p, 6);
      p += 6;
      continue;
    }

    if (strncmp(p, "_Alignof", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_ALIGNOF, cur, p, 8);
      p += 8;
      continue;
    }

    if (strncmp(p, "struct", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_STRUCT, cur, p, 6);
      p += 6;
      continue;
    }

    if (strncmp(p, "enum", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_ENUM, cur, p, 4);
      p += 4;
      continue;
    }

    if (strncmp(p, "switch", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_SWITCH, cur, p, 6);
      p += 6;
      continue;
    }

    if (strncmp(p, "if", 2) == 0 && !is_alnum(p[2])) {
      cur = new_token(TK_IF, cur, p, 2);
      p += 2;
      continue;
    }

    if (strncmp(p, "else", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_ELSE, cur, p, 4);
      p += 4;
      continue;
    }

    if (strncmp(p, "do", 2) == 0 && !is_alnum(p[2])) {
      cur = new_token(TK_DO, cur, p, 2);
      p += 2;
      continue;
    }

    if (strncmp(p, "while", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_WHILE, cur, p, 5);
      p += 5;
      continue;
    }

    if (strncmp(p, "for", 3) == 0 && !is_alnum(p[3])) {
      cur = new_token(TK_FOR, cur, p, 3);
      p += 3;
      continue;
    }

    if (strncmp(p, "break", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_BREAK, cur, p, 5);
      p += 5;
      continue;
    }

    if (strncmp(p, "continue", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_CONTINUE, cur, p, 8);
      p += 8;
      continue;
    }

    if (strncmp(p, "default", 7) == 0 && !is_alnum(p[7])) {
      cur = new_token(TK_DEFAULT, cur, p, 7);
      p += 7;
      continue;
    }

    if (strncmp(p, "case", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_CASE, cur, p, 4);
      p += 4;
      continue;
    }

    if (strncmp(p, "const", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_CONST, cur, p, 5);
      p += 5;
      continue;
    }

    if (strncmp(p, "extern", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_EXTERN, cur, p, 6);
      p += 6;
      continue;
    }

    if (strncmp(p, "static", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_STATIC, cur, p, 6);
      p += 6;
      continue;
    }

    if (strncmp(p, "void", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_VOID, cur, p, 4);
      p += 4;
      continue;
    }

    if (strncmp(p, "int", 3) == 0 && !is_alnum(p[3])) {
      cur = new_token(TK_INT, cur, p, 3);
      p += 3;
      continue;
    }

    if (strncmp(p, "char", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_CHAR, cur, p, 4);
      p += 4;
      continue;
    }

    if (strspn(p, variable_letters) > 0) {
      int len = strspn(p, variable_letters);
      cur = new_token(TK_IDENT, cur, p, len);
      p += len;
      continue;
    }

    error_at(p, "cannot tokenize");
  }

  new_token(TK_EOF, cur, p, 0);
  return head.next;
}

void debug_token(Token *token) {
  Token *cur = token;
  while (cur != NULL) {
    fprintf(stderr, "kind:%d , len :%d , str: %.*s\n", cur->kind, cur->len,
            cur->len, cur->str);
    cur = cur->next;
  }
}
