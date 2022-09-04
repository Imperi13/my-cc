
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "file.h"
#include "preprocess.h"
#include "str_dict.h"
#include "tokenize.h"

// when post == NULL, the line is just consumed, not executed

static void process_macro_group(Token **post, Token **pre, Token *tok);
static void process_if_group(Token **post, Token **pre, Token *tok);
static void process_elif_group(Token **post, Token **pre, Token *tok,
                               bool already_true);
static void process_else_group(Token **post, Token **pre, Token *tok,
                               bool already_true);
static void process_include_line(Token **post, Token **pre, Token *tok);
static void process_define_line(Token **post, Token **pre, Token *tok);
static void process_undef_line(Token **post, Token **pre, Token *tok);
static void process_pragma_line(Token **post, Token **pre, Token *tok);
static void process_text_line(Token **post, Token **pre, Token *tok);

static void expand_define(Token **pre, Token *tok);
static void add_predefine(char *name, char *replace);

static void consume_line(Token **pre, Token *tok);

static long process_constant(Token **pre, Token *tok);
static long process_conditional(Token **pre, Token *tok);
static long process_logical_or(Token **pre, Token *tok);
static long process_logical_and(Token **pre, Token *tok);
static long process_bit_or(Token **pre, Token *tok);
static long process_bit_xor(Token **pre, Token *tok);
static long process_bit_and(Token **pre, Token *tok);
static long process_equality(Token **pre, Token *tok);
static long process_relational(Token **pre, Token *tok);
static long process_shift(Token **pre, Token *tok);
static long process_add(Token **pre, Token *tok);
static long process_mul(Token **ptr, Token *tok);
static long process_cast(Token **pre, Token *tok);
static long process_unary(Token **pre, Token *tok);
static long process_postfix(Token **pre, Token *tok);
static long process_primary(Token **pre, Token *tok);

typedef struct PragmaOnceList PragmaOnceList;
struct PragmaOnceList {
  char *filepath;

  // for linked-list
  PragmaOnceList *next;
};

typedef struct DefineList DefineList;
struct DefineList {
  char *name;

  // startからTK_NEWLINEまでが展開するトークン
  Token *start;
};

PragmaOnceList *pragma_list = NULL;
StrDict *define_dict = NULL;

bool is_included(char *filepath) {
  for (PragmaOnceList *cur = pragma_list; cur; cur = cur->next)
    if (strncmp(cur->filepath, filepath, strlen(filepath)) == 0)
      return true;
  return false;
}

// insert token sequence
// dst -> dst_next , src -> ... -> src_end -> TK_EOF
// dst -> src -> ... -> src_end -> dst_next
void insert_token_seq(Token *dst, Token *src) {
  Token *dst_next = dst->next;

  Token *cur = src;
  while (cur->next->kind != TK_EOF)
    cur = cur->next;

  dst->next = src;
  cur->next = dst_next;
}

bool is_if_group(Token *tok) {
  return cmp_ident(tok->next, "ifdef") || cmp_ident(tok->next, "ifndef") ||
         equal_kind(tok->next, TK_IF);
}

void process_macro_group(Token **post, Token **pre, Token *tok) {
  if (!equal(tok, "#"))
    error_token(tok, "this line is not macro");

  if (is_if_group(tok)) {
    process_if_group(post, pre, tok);
  } else if (cmp_ident(tok->next, "include")) {
    process_include_line(post, pre, tok);
  } else if (cmp_ident(tok->next, "define")) {
    process_define_line(post, pre, tok);
  } else if (cmp_ident(tok->next, "undef")) {
    process_undef_line(post, pre, tok);
  } else if (cmp_ident(tok->next, "pragma")) {
    process_pragma_line(post, pre, tok);
  } else {
    // warn("ignore unrecognized macro group");
    // consume_line(pre, tok);
    not_implemented_token(tok);
  }
}

void process_if_group(Token **post, Token **pre, Token *tok) {
  consume(&tok, tok, "#");
  if (cmp_ident(tok, "ifdef")) {
    expect_ident(&tok, tok, "ifdef");

    char *define_str = getname_ident(&tok, tok);
    bool cond = (find_str_dict(define_dict, define_str) != NULL);
    expect_kind(&tok, tok, TK_NEWLINE);

    while (!equal(tok, "#") ||
           !(cmp_ident(tok->next, "endif") || cmp_ident(tok->next, "elif") ||
             equal_kind(tok->next, TK_ELSE))) {
      if (equal(tok, "#"))
        process_macro_group(cond ? post : NULL, &tok, tok);
      else
        process_text_line(cond ? post : NULL, &tok, tok);
    }

    if (cmp_ident(tok->next, "elif")) {
      process_elif_group(post, pre, tok, cond);
      return;
    } else if (equal_kind(tok->next, TK_ELSE)) {
      process_else_group(post, pre, tok, cond);
      return;
    }

    expect(&tok, tok, "#");
    expect_ident(&tok, tok, "endif");
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else if (cmp_ident(tok, "ifndef")) {
    expect_ident(&tok, tok, "ifndef");

    char *define_str = getname_ident(&tok, tok);
    bool cond = (find_str_dict(define_dict, define_str) == NULL);
    expect_kind(&tok, tok, TK_NEWLINE);

    while (!equal(tok, "#") ||
           !(cmp_ident(tok->next, "endif") || cmp_ident(tok->next, "elif") ||
             equal_kind(tok->next, TK_ELSE))) {
      if (equal(tok, "#"))
        process_macro_group(cond ? post : NULL, &tok, tok);
      else
        process_text_line(cond ? post : NULL, &tok, tok);
    }

    if (cmp_ident(tok->next, "elif")) {
      process_elif_group(post, pre, tok, cond);
      return;
    } else if (equal_kind(tok->next, TK_ELSE)) {
      process_else_group(post, pre, tok, cond);
      return;
    }

    expect(&tok, tok, "#");
    expect_ident(&tok, tok, "endif");
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else if (equal_kind(tok, TK_IF)) {
    consume_kind(&tok, tok, TK_IF);
    bool cond = (process_constant(&tok, tok) != 0);
    expect_kind(&tok, tok, TK_NEWLINE);

    while (!equal(tok, "#") ||
           !(cmp_ident(tok->next, "endif") || cmp_ident(tok->next, "elif") ||
             equal_kind(tok->next, TK_ELSE))) {
      if (equal(tok, "#"))
        process_macro_group(cond ? post : NULL, &tok, tok);
      else
        process_text_line(cond ? post : NULL, &tok, tok);
    }

    if (cmp_ident(tok->next, "elif")) {
      process_elif_group(post, pre, tok, cond);
      return;
    } else if (equal_kind(tok->next, TK_ELSE)) {
      process_else_group(post, pre, tok, cond);
      return;
    }

    expect(&tok, tok, "#");
    expect_ident(&tok, tok, "endif");
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else {
    not_implemented_token(tok);
  }
}

void process_elif_group(Token **post, Token **pre, Token *tok,
                        bool already_true) {
  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "elif");

  bool cond = (process_constant(&tok, tok) != 0);
  expect_kind(&tok, tok, TK_NEWLINE);

  while (!equal(tok, "#") ||
         !(cmp_ident(tok->next, "endif") || cmp_ident(tok->next, "elif") ||
           equal_kind(tok->next, TK_ELSE))) {
    if (equal(tok, "#"))
      process_macro_group((!already_true && cond) ? post : NULL, &tok, tok);
    else
      process_text_line((!already_true && cond) ? post : NULL, &tok, tok);
  }

  if (cmp_ident(tok->next, "elif")) {
    process_elif_group(post, pre, tok, already_true || cond);
    return;
  } else if (equal_kind(tok->next, TK_ELSE)) {
    process_else_group(post, pre, tok, already_true || cond);
    return;
  }

  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "endif");
  expect_kind(&tok, tok, TK_NEWLINE);

  *pre = tok;
}

void process_else_group(Token **post, Token **pre, Token *tok,
                        bool already_true) {
  expect(&tok, tok, "#");
  expect_kind(&tok, tok, TK_ELSE);
  expect_kind(&tok, tok, TK_NEWLINE);

  while (!equal(tok, "#") || !cmp_ident(tok->next, "endif")) {
    if (equal(tok, "#"))
      process_macro_group((!already_true) ? post : NULL, &tok, tok);
    else
      process_text_line((!already_true) ? post : NULL, &tok, tok);
  }

  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "endif");
  expect_kind(&tok, tok, TK_NEWLINE);

  *pre = tok;
}

void process_include_line(Token **post, Token **pre, Token *tok) {
  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "include");

  if (equal(tok, "<")) {
    // ignore include
    consume(&tok, tok, "<");

    Token *filename_start = tok;
    int len = 0;
    for (Token *cur = filename_start; !equal(cur, ">"); cur = cur->next) {
      len += cur->len;
    }

    char *filename = calloc(len + 1, sizeof(char));
    int off = 0;

    while (!equal(tok, ">")) {
      memcpy(filename + off, tok->str, tok->len);
      off += tok->len;
      tok = tok->next;
    }

    expect(&tok, tok, ">");

    if (post) {
      char *filepath = calloc(PATH_MAX + 1, sizeof(char));
      snprintf(filepath, PATH_MAX, "/usr/local/musl/include/%s", filename);

      /*
      fprintf(stderr, "%s\n", filepath);
      warn_token(filename_start, "ignore include");
      */

      if (!is_included(filepath)) {
        char *buf = read_file(filepath);
        Token *inc_tok = tokenize(buf, filepath);

        insert_token_seq(tok, inc_tok);
      }
    }

    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else if (equal_kind(tok, TK_STR)) {
    Token *file = consume_kind(&tok, tok, TK_STR);
    char *filepath = file->str_literal->str;

    if (post && !is_included(filepath)) {
      char *buf = read_file(filepath);
      Token *inc_tok = tokenize(buf, filepath);

      insert_token_seq(tok, inc_tok);
    }

    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else {
    not_implemented(__func__);
  }
}

void process_define_line(Token **post, Token **pre, Token *tok) {
  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "define");

  if (!post) {
    consume_line(pre, tok);
    return;
  }

  char *define_str = getname_ident(&tok, tok);

  DefineList *new_def = calloc(1, sizeof(DefineList));
  new_def->name = define_str;

  Token *head = calloc(1, sizeof(Token));
  Token *cur = head;

  while (expand_define(&tok, tok), tok->kind != TK_NEWLINE) {
    cur->next = tok;
    if (equal_kind(tok, TK_IDENT) && strcmp(tok->ident_str, define_str) == 0) {
      tok->is_recursived = true;
    }
    cur = cur->next;

    tok = tok->next;
  }
  cur->next = tok;

  new_def->start = head->next;

  if (post) {
    if (find_str_dict(define_dict, define_str)) {
      warn("redifine %s", define_str);
      remove_str_dict(define_dict, define_str);
    }
    add_str_dict(define_dict, define_str, new_def);
  }

  expect_kind(&tok, tok, TK_NEWLINE);
  *pre = tok;
}

void process_undef_line(Token **post, Token **pre, Token *tok) {
  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "undef");

  char *define_str = getname_ident(&tok, tok);

  if (post) {
    if (find_str_dict(define_dict, define_str))
      remove_str_dict(define_dict, define_str);
  }

  expect_kind(&tok, tok, TK_NEWLINE);
  *pre = tok;
}

// 先頭tokenが展開できなくなるまで繰り返す
// 最悪でもTK_NEWLINEで止まる
void expand_define(Token **pre, Token *tok) {
  if (!equal_kind(tok, TK_IDENT)) {
    *pre = tok;
    return;
  }

  if (tok->is_recursived) {
    *pre = tok;
    return;
  }

  DefineList *def = find_str_dict(define_dict, tok->ident_str);
  if (!def) {
    *pre = tok;
    return;
  }

  Token *symbol = tok;
  Token *sym_next = tok->next;
  Token *cur = symbol;
  for (Token *src = def->start; src->kind != TK_NEWLINE; src = src->next) {
    Token *cpy = calloc(1, sizeof(Token));
    memcpy(cpy, src, sizeof(Token));
    cur->next = cpy;
    cur = cur->next;
  }
  cur->next = sym_next;
  expand_define(pre, symbol->next);
}

void add_predefine(char *name, char *replace) {
  int size = strlen(replace);
  char *buf = calloc(size + 2, sizeof(char));
  strcpy(buf, replace);
  buf[size] = '\n';

  Token *start = tokenize(buf, "predefined_macro");

  DefineList *def = calloc(1, sizeof(DefineList));
  def->name = name;
  def->start = start;

  add_str_dict(define_dict, name, def);
}

void process_pragma_line(Token **post, Token **pre, Token *tok) {
  expect(&tok, tok, "#");
  expect_ident(&tok, tok, "pragma");

  if (cmp_ident(tok, "once")) {
    expect_ident(&tok, tok, "once");
    if (post) {

      PragmaOnceList *pragma = calloc(1, sizeof(PragmaOnceList));
      pragma->filepath = tok->filepath;
      pragma->next = pragma_list;
      pragma_list = pragma;
    }

    expect_kind(&tok, tok, TK_NEWLINE);
    *pre = tok;
  } else {
    not_implemented_token(tok);
  }
}

void process_text_line(Token **post, Token **pre, Token *tok) {
  if (equal(tok, "#"))
    error_token(tok, "this line is not text line");

  while (expand_define(&tok, tok), tok->kind != TK_NEWLINE) {
    if (post) {
      (*post)->next = tok;
      (*post) = (*post)->next;
    }
    tok = tok->next;
  }

  // append TK_NEWLINE
  if (post) {
    (*post)->next = tok;
    (*post) = (*post)->next;
  }

  // skip TK_NEWLINE
  *pre = tok->next;
}

void consume_line(Token **pre, Token *tok) {
  while (tok->kind != TK_NEWLINE)
    tok = tok->next;
  *pre = tok->next;
}

long process_constant(Token **pre, Token *tok) {
  return process_conditional(pre, tok);
}

long process_conditional(Token **pre, Token *tok) {
  long cond = process_logical_or(&tok, tok);

  expand_define(&tok, tok);
  if (equal(tok, "?")) {
    consume(&tok, tok, "?");
    long lhs = process_conditional(&tok, tok);
    expand_define(&tok, tok);
    expect(&tok, tok, ":");
    long rhs = process_conditional(&tok, tok);

    *pre = tok;
    return cond ? lhs : rhs;
  }

  *pre = tok;
  return cond;
}

long process_logical_or(Token **pre, Token *tok) {
  long lhs = process_logical_and(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "||")) {
      consume(&tok, tok, "||");
      long rhs = process_logical_and(&tok, tok);
      lhs = lhs || rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_logical_and(Token **pre, Token *tok) {
  long lhs = process_bit_or(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "&&")) {
      consume(&tok, tok, "&&");
      long rhs = process_bit_or(&tok, tok);
      lhs = lhs && rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_bit_or(Token **pre, Token *tok) {
  long lhs = process_bit_xor(&tok, tok);
  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "|")) {
      consume(&tok, tok, "|");
      long rhs = process_bit_xor(&tok, tok);
      lhs = lhs | rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_bit_xor(Token **pre, Token *tok) {
  long lhs = process_bit_and(&tok, tok);
  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "^")) {
      consume(&tok, tok, "^");
      long rhs = process_bit_and(&tok, tok);
      lhs = lhs ^ rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_bit_and(Token **pre, Token *tok) {
  long lhs = process_equality(&tok, tok);
  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "&")) {
      consume(&tok, tok, "&");
      long rhs = process_equality(&tok, tok);
      lhs = lhs & rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_equality(Token **pre, Token *tok) {
  long lhs = process_relational(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "==")) {
      consume(&tok, tok, "==");
      long rhs = process_relational(&tok, tok);
      lhs = lhs == rhs;
    } else if (equal(tok, "!=")) {
      consume(&tok, tok, "!=");
      long rhs = process_relational(&tok, tok);
      lhs = lhs != rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_relational(Token **pre, Token *tok) {
  long lhs = process_shift(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "<")) {
      consume(&tok, tok, "<");
      long rhs = process_shift(&tok, tok);
      lhs = lhs < rhs;
    } else if (equal(tok, "<=")) {
      consume(&tok, tok, "<=");
      long rhs = process_shift(&tok, tok);
      lhs = lhs <= rhs;
    } else if (equal(tok, ">")) {
      consume(&tok, tok, ">");
      long rhs = process_shift(&tok, tok);
      lhs = lhs > rhs;
    } else if (equal(tok, ">=")) {
      consume(&tok, tok, ">=");
      long rhs = process_shift(&tok, tok);
      lhs = lhs >= rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_shift(Token **pre, Token *tok) {
  long lhs = process_add(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "<<")) {
      consume(&tok, tok, "<<");
      long rhs = process_add(&tok, tok);
      lhs = lhs << rhs;
    } else if (equal(tok, ">>")) {
      consume(&tok, tok, ">>");
      long rhs = process_add(&tok, tok);
      lhs = lhs >> rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_add(Token **pre, Token *tok) {
  long lhs = process_mul(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "+")) {
      consume(&tok, tok, "+");
      long rhs = process_mul(&tok, tok);
      lhs = lhs + rhs;
    } else if (equal(tok, "-")) {
      consume(&tok, tok, "-");
      long rhs = process_mul(&tok, tok);
      lhs = lhs - rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_mul(Token **pre, Token *tok) {
  long lhs = process_cast(&tok, tok);

  for (;;) {
    expand_define(&tok, tok);
    if (equal(tok, "*")) {
      consume(&tok, tok, "*");
      long rhs = process_cast(&tok, tok);
      lhs = lhs * rhs;
    } else if (equal(tok, "/")) {
      consume(&tok, tok, "/");
      long rhs = process_cast(&tok, tok);
      lhs = lhs / rhs;
    } else if (equal(tok, "%")) {
      consume(&tok, tok, "%");
      long rhs = process_cast(&tok, tok);
      lhs = lhs % rhs;
    } else {
      *pre = tok;
      return lhs;
    }
  }
}

long process_cast(Token **pre, Token *tok) { return process_unary(pre, tok); }

long process_unary(Token **pre, Token *tok) {
  expand_define(&tok, tok);
  if (equal(tok, "+")) {
    consume(&tok, tok, "+");
    return process_cast(pre, tok);
  }

  if (equal(tok, "-")) {
    consume(&tok, tok, "-");
    return -process_cast(pre, tok);
  }

  if (equal(tok, "!")) {
    consume(&tok, tok, "!");
    return !process_cast(pre, tok);
  }

  if (equal(tok, "~")) {
    consume(&tok, tok, "~");
    return ~process_cast(pre, tok);
  }

  if (cmp_ident(tok, "defined")) {
    expect_ident(&tok, tok, "defined");

    long ret;

    if (equal(tok, "(")) {
      consume(&tok, tok, "(");
      char *name = getname_ident(&tok, tok);
      expect(&tok, tok, ")");

      ret = find_str_dict(define_dict, name) ? 1 : 0;
    } else {
      char *name = getname_ident(&tok, tok);

      ret = find_str_dict(define_dict, name) ? 1 : 0;
    }

    *pre = tok;
    return ret;
  }

  return process_postfix(pre, tok);
}

long process_postfix(Token **pre, Token *tok) {
  return process_primary(pre, tok);
}

long process_primary(Token **pre, Token *tok) {
  expand_define(&tok, tok);
  if (tok->kind == TK_NUM) {
    *pre = tok->next;
    return tok->val;
  } else if (tok->kind == TK_IDENT) {
    *pre = tok->next;
    return 0;
  } else if (equal(tok, "(")) {
    consume(&tok, tok, "(");
    long ret = process_conditional(&tok, tok);
    expand_define(&tok, tok);
    expect(&tok, tok, ")");
    *pre = tok;
    return ret;
  } else {
    not_implemented_token(tok);
    return 0;
  }
}

Token *preprocess(Token *tok) {
  define_dict = new_str_dict();
  pragma_list = NULL;

  add_predefine("__STDC_VERSION__", "201112L");
  add_predefine("__STDC__", "1");

  Token *head = calloc(1, sizeof(Token));
  Token *cur = head;

  while (!at_eof(tok)) {
    if (equal(tok, "#")) {
      process_macro_group(&cur, &tok, tok);
    } else {
      process_text_line(&cur, &tok, tok);
    }
  }

  define_dict = NULL;

  cur->next = tok;
  return head->next;
}

Token *remove_newline(Token *tok) {
  Token *head = calloc(1, sizeof(Token));
  Token *cur = head;

  while (tok->kind != TK_EOF) {
    if (tok->kind != TK_NEWLINE) {
      cur->next = tok;
      cur = cur->next;
    }
    tok = tok->next;
  }

  cur->next = tok;

  return head->next;
}
