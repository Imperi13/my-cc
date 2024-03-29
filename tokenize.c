#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "tokenize.h"

StrLiteral *str_literals = NULL;
const char *variable_letters =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

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
    error_token(token, "not '%s' op", op);
  *rest = token->next;
}

Token *expect_kind(Token **rest, Token *token, TokenKind kind) {
  if (token->kind != kind)
    error_token(token, "not expected TokenKind %d", kind);
  *rest = token->next;
  return token;
}

int expect_number(Token **rest, Token *token) {
  if (token->kind != TK_NUM)
    error_token(token, "not number");
  int val = token->val;
  *rest = token->next;
  return val;
}

void expect_ident(Token **rest, Token *token, char *name) {
  if (token->kind != TK_IDENT)
    error_token(token, "not ident");
  if (strcmp(token->ident_str, name) != 0)
    error_token(token, "must be ident \"%s\"", name);
  *rest = token->next;
}

char *getname_ident(Token **rest, Token *tok) {
  if (!equal_kind(tok, TK_IDENT))
    error_token(tok, "not ident token");
  *rest = tok->next;
  return tok->ident_str;
}

bool cmp_ident(Token *tok, const char *name) {
  if (tok->kind != TK_IDENT)
    return false;
  return strcmp(tok->ident_str, name) == 0;
}

bool at_eof(Token *token) { return token->kind == TK_EOF; }

bool is_same_token(Token *a, Token *b) {
  if (a->kind != b->kind)
    return false;
  if (a->kind == TK_RESERVED || a->kind == TK_IDENT || a->kind == TK_STR ||
      a->kind == TK_NUM) {
    if (strncmp(a->str, b->str, a->len) != 0)
      return false;
    return true;
  }
  return true;
}

Token *new_token(TokenKind kind, Token *cur, char *str, int len, char *filepath,
                 char *file_buf) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
  tok->filepath = filepath;
  tok->file_buf = file_buf;
  cur->next = tok;
  return tok;
}

Token *new_eof_token(void) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = TK_EOF;
  return tok;
}

Token *copy_and_consume(Token **rest, Token *tok) {
  Token *tmp = calloc(1, sizeof(Token));
  memcpy(tmp, tok, sizeof(Token));
  // cut new-token from token-seq
  tmp->next = NULL;
  *rest = tok->next;
  return tmp;
}

Token *copy_token_seq(Token *tok) {
  Token head = {.next = NULL};
  Token *copy_cur = &head;

  for (Token *cur = tok; cur->kind != TK_EOF; cur = cur->next) {
    Token *copy = calloc(1, sizeof(Token));
    memcpy(copy, cur, sizeof(Token));
    copy_cur->next = copy;
    copy_cur = copy_cur->next;
  }

  copy_cur->next = new_eof_token();
  return head.next;
}

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

int digit_base(char t, int base) {
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

unsigned long long num_literal(char *p, char **rest) {
  int base;
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

  unsigned long long num = 0;
  while (digit_base(*start, base) >= 0) {
    num *= base;
    num += digit_base(*start, base);
    start++;
  }
  *rest = start;
  return num;
}

void consume_num_suffix(char **rest, char *p, Token *num_tok) {
  while ((*p == 'l' || *p == 'L') ||
         (strncmp(p, "ll", 2) == 0 || strncmp(p, "LL", 2) == 0) ||
         (*p == 'u' || *p == 'U')) {
    if (*p == 'l' || *p == 'L') {
      num_tok->is_long = true;
      p++;
    } else if (strncmp(p, "ll", 2) == 0 || strncmp(p, "LL", 2) == 0) {
      num_tok->is_longlong = true;
      p += 2;
    } else if (*p == 'u' || *p == 'U') {
      num_tok->is_unsigned = true;
      p++;
    } else
      error("invalid num_suffix");
  }

  *rest = p;
}

char consume_char(char **rest, char *p) {
  if (*p == '\\') {
    char ret;
    p++;
    if (*p == 'e') {
      ret = '\e';
      p++;
    } else if (*p == 't') {
      ret = '\t';
      p++;
    } else if (*p == 'n') {
      ret = '\n';
      p++;
    } else if (*p == '\\') {
      ret = '\\';
      p++;
    } else if (*p == '\'') {
      ret = '\'';
      p++;
    } else if (*p == '\"') {
      ret = '\"';
      p++;
    } else if (*p == '0') {
      ret = '\0';
      p++;
    } else if (*p == 'x') {
      p++;
      long num = 0;
      while (digit_base(*p, 16) >= 0) {
        num *= 16;
        num += digit_base(*p, 16);
        p++;

        if (num >= 256)
          error("overflow char-literal");
      }

      ret = num;
    } else {
      not_implemented(__func__);
    }

    *rest = p;
    return ret;
  } else {
    char ret = *p;
    p++;

    *rest = p;
    return ret;
  }
}

Token *tokenize(char *p, char *filepath) {
  char *file_buf = p;
  Token head = {.next = NULL};
  Token *cur = &head;

  while (*p) {
    if (isblank(*p)) {
      p++;
      continue;
    }

    if (*p == '\\') {
      p++;
      if (*p != '\n')
        error("cannot tokenize backslash");
      p++;
      continue;
    }

    if (*p == '\n' || *p == '\x0c') {
      cur = new_token(TK_NEWLINE, cur, p, 1, filepath, file_buf);
      p++;
      continue;
    }

    if (*p == '\'') {
      char *tok_start = p;
      p++;
      char val = consume_char(&p, p);
      if (*p != '\'')
        error("not find ' \n%s", tok_start);
      p++;

      cur =
          new_token(TK_NUM, cur, tok_start, p - tok_start, filepath, file_buf);
      cur->val = val;
      continue;
    }

    if (*p == '"') {
      char *tok_start = p;
      char *str_cur = tok_start + 1;
      int len = 0;
      while (*str_cur != '"') {
        len++;
        consume_char(&str_cur, str_cur);
      }

      StrLiteral *push_literal = calloc(1, sizeof(StrLiteral));
      char *str = calloc(len + 1, sizeof(char));
      push_literal->str = str;
      push_literal->len = len;

      str_cur = tok_start + 1;
      int i = 0;
      while (*str_cur != '"') {
        str[i] = consume_char(&str_cur, str_cur);
        i++;
      }

      Token *tmp = new_token(TK_STR, cur, tok_start, str_cur - tok_start + 1,
                             filepath, file_buf);
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
        error("not found comment end */ \n%s", p);
      p = q + 2;
      continue;
    }

    if (strncmp(p, "<<=", 3) == 0 || strncmp(p, ">>=", 3) == 0 ||
        strncmp(p, "...", 3) == 0) {
      cur = new_token(TK_RESERVED, cur, p, 3, filepath, file_buf);
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
      cur = new_token(TK_RESERVED, cur, p, 2, filepath, file_buf);
      p += 2;
      continue;
    }

    if (strchr("+-*/|&!~^%:?.,;=(){}[]<>#", *p)) {
      cur = new_token(TK_RESERVED, cur, p, 1, filepath, file_buf);
      p++;
      continue;
    }

    if (isdigit(*p)) {
      // TODO floating constants
      char *prev = p;
      cur = new_token(TK_NUM, cur, p, 1, filepath, file_buf);
      cur->val = num_literal(p, &p);
      if (*p == '.' || *p == 'e' || *p == 'E' || *p == 'p' || *p == 'P') {
        p = prev;
        cur->is_floatint_constant = true;
        cur->floating_val = strtod(p, &p);
      } else {
        consume_num_suffix(&p, p, cur);
      }

      cur->len = p - prev;
      continue;
    }

    if (strncmp(p, "inline", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_INLINE, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "_Noreturn", 9) == 0 && !is_alnum(p[9])) {
      cur = new_token(TK_NORETURN, cur, p, 9, filepath, file_buf);
      p += 9;
      continue;
    }

    if (strncmp(p, "unsigned", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_UNSIGNED, cur, p, 8, filepath, file_buf);
      p += 8;
      continue;
    }

    if (strncmp(p, "signed", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_SIGNED, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "float", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_FLOAT, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "double", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_DOUBLE, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "short", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_SHORT, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "_Bool", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_BOOL, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "typedef", 7) == 0 && !is_alnum(p[7])) {
      cur = new_token(TK_TYPEDEF, cur, p, 7, filepath, file_buf);
      p += 7;
      continue;
    }

    if (strncmp(p, "return", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_RETURN, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "goto", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_GOTO, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strncmp(p, "sizeof", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_SIZEOF, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "_Alignof", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_ALIGNOF, cur, p, 8, filepath, file_buf);
      p += 8;
      continue;
    }

    if (strncmp(p, "struct", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_STRUCT, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "union", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_UNION, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "enum", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_ENUM, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strncmp(p, "switch", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_SWITCH, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "if", 2) == 0 && !is_alnum(p[2])) {
      cur = new_token(TK_IF, cur, p, 2, filepath, file_buf);
      p += 2;
      continue;
    }

    if (strncmp(p, "else", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_ELSE, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strncmp(p, "do", 2) == 0 && !is_alnum(p[2])) {
      cur = new_token(TK_DO, cur, p, 2, filepath, file_buf);
      p += 2;
      continue;
    }

    if (strncmp(p, "while", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_WHILE, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "for", 3) == 0 && !is_alnum(p[3])) {
      cur = new_token(TK_FOR, cur, p, 3, filepath, file_buf);
      p += 3;
      continue;
    }

    if (strncmp(p, "break", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_BREAK, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "continue", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_CONTINUE, cur, p, 8, filepath, file_buf);
      p += 8;
      continue;
    }

    if (strncmp(p, "default", 7) == 0 && !is_alnum(p[7])) {
      cur = new_token(TK_DEFAULT, cur, p, 7, filepath, file_buf);
      p += 7;
      continue;
    }

    if (strncmp(p, "case", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_CASE, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strncmp(p, "const", 5) == 0 && !is_alnum(p[5])) {
      cur = new_token(TK_CONST, cur, p, 5, filepath, file_buf);
      p += 5;
      continue;
    }

    if (strncmp(p, "restrict", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_RESTRICT, cur, p, 8, filepath, file_buf);
      p += 8;
      continue;
    }

    if (strncmp(p, "volatile", 8) == 0 && !is_alnum(p[8])) {
      cur = new_token(TK_VOLATILE, cur, p, 8, filepath, file_buf);
      p += 8;
      continue;
    }

    if (strncmp(p, "extern", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_EXTERN, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "static", 6) == 0 && !is_alnum(p[6])) {
      cur = new_token(TK_STATIC, cur, p, 6, filepath, file_buf);
      p += 6;
      continue;
    }

    if (strncmp(p, "void", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_VOID, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strncmp(p, "long", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_LONG, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strncmp(p, "int", 3) == 0 && !is_alnum(p[3])) {
      cur = new_token(TK_INT, cur, p, 3, filepath, file_buf);
      p += 3;
      continue;
    }

    if (strncmp(p, "char", 4) == 0 && !is_alnum(p[4])) {
      cur = new_token(TK_CHAR, cur, p, 4, filepath, file_buf);
      p += 4;
      continue;
    }

    if (strspn(p, variable_letters) > 0) {
      int len = strspn(p, variable_letters);
      cur = new_token(TK_IDENT, cur, p, len, filepath, file_buf);
      cur->ident_str = calloc(len + 1, sizeof(char));
      memcpy(cur->ident_str, p, len);
      p += len;
      continue;
    }

    error("cannot tokenize \n%s", p);
  }

  new_token(TK_EOF, cur, p, 0, filepath, file_buf);
  return head.next;
}

void print_token_seq(FILE *output_stream, Token *tok) {
  for (Token *cur = tok; cur->kind != TK_EOF; cur = cur->next)
    fprintf(output_stream, "%.*s ", (int)cur->len, cur->str);
}

void debug_token(Token *token) {
  Token *cur = token;
  while (cur != NULL) {
    fprintf(stderr, "kind:%d , len :%lu , str: %.*s\n", cur->kind, cur->len,
            (int)cur->len, cur->str);
    cur = cur->next;
  }
}
