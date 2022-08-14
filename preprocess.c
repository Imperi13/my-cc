
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "file.h"
#include "preprocess.h"
#include "tokenize.h"

#ifndef __STDC__

int strncmp();
typedef int size_t;
size_t strlen();
int memcmp();
void *calloc();

#endif

static void process_macro_group(Token **post, Token **pre, Token *tok);
static void process_if_group(Token **post, Token **pre, Token *tok);
static void process_include_line(Token **post, Token **pre, Token *tok);
static void process_define_line(Token **post, Token **pre, Token *tok);
static void process_pragma_line(Token **post, Token **pre, Token *tok);
static void process_text_line(Token **post, Token **pre, Token *tok);

static void expand_define(Token **pre, Token *tok);

static void consume_line(Token **pre, Token *tok);

typedef struct PragmaOnceList PragmaOnceList;
struct PragmaOnceList {
  char *filepath;

  // for linked-list
  PragmaOnceList *next;
};

typedef struct DefineList DefineList;
struct DefineList {
  char *sym_name;
  int sym_len;

  Token *start;

  // for linked-list
  DefineList *next;
};

PragmaOnceList *pragma_list;
DefineList *define_list;

bool is_included(char *filepath) {
  for (PragmaOnceList *cur = pragma_list; cur; cur = cur->next)
    if (strncmp(cur->filepath, filepath, strlen(filepath)) == 0)
      return 1;
  // return true;
  return 0;
  // return false;
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

DefineList *find_define(char *def_name, int def_len) {
  for (DefineList *cur = define_list; cur; cur = cur->next)
    if (cur->sym_len == def_len && !memcmp(def_name, cur->sym_name, def_len))
      return cur;
  return 0;
  // return NULL;
}

bool cmp_ident(Token *tok, const char *name) {
  return strncmp(tok->str, name, tok->len) == 0;
}

bool is_if_group(Token *tok) {
  return cmp_ident(tok->next, "ifdef") || cmp_ident(tok->next, "ifndef");
}

void process_macro_group(Token **post, Token **pre, Token *tok) {
  if (!equal(tok, "#"))
    error_at(tok->str, "this line is not macro");

  if (is_if_group(tok)) {
    process_if_group(post, pre, tok);
  } else if (cmp_ident(tok->next, "include")) {
    process_include_line(0, pre, tok);
    // process_include_line(NULL, pre, tok);
  } else if (cmp_ident(tok->next, "define")) {
    process_define_line(0, pre, tok);
    // process_define_line(NULL,pre,tok);
  } else if (cmp_ident(tok->next, "pragma")) {
    process_pragma_line(post, pre, tok);
  } else {
    not_implemented_at(tok->str);
  }
}

void process_if_group(Token **post, Token **pre, Token *tok) {
  consume(&tok, tok, "#");
  if (cmp_ident(tok, "ifdef")) {
    expect_kind(&tok, tok, TK_IDENT);

    Token *cond_tok = consume_kind(&tok, tok, TK_IDENT);
    bool cond = (find_define(cond_tok->str, cond_tok->len) != 0);
    // bool cond = (find_define(cond_tok->str, cond_tok->len) != NULL);
    expect_kind(&tok, tok, TK_NEWLINE);

    while (!equal(tok, "#") || !cmp_ident(tok->next, "endif")) {
      if (cond) {
        if (equal(tok, "#"))
          process_macro_group(post, &tok, tok);
        else
          process_text_line(post, &tok, tok);
      } else {
        consume_line(&tok, tok);
      }
    }

    expect(&tok, tok, "#");
    consume_kind(&tok, tok, TK_IDENT);
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else if (cmp_ident(tok, "ifndef")) {
    expect_kind(&tok, tok, TK_IDENT);

    Token *cond_tok = consume_kind(&tok, tok, TK_IDENT);
    bool cond = (find_define(cond_tok->str, cond_tok->len) == 0);
    // bool cond = (find_define(cond_tok->str, cond_tok->len) == NULL);
    expect_kind(&tok, tok, TK_NEWLINE);

    Token *dummy;
    while (!equal(tok, "#") || !cmp_ident(tok->next, "endif")) {
      if (cond) {
        if (equal(tok, "#"))
          process_macro_group(post, &tok, tok);
        else
          process_text_line(post, &tok, tok);
      } else {
        consume_line(&tok, tok);
      }
    }

    expect(&tok, tok, "#");
    if (!cmp_ident(tok, "endif"))
      not_implemented_at("only ifndef~endif");
    consume_kind(&tok, tok, TK_IDENT);
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else {
    not_implemented_at(tok->str);
  }
}

void process_include_line(Token **post, Token **pre, Token *tok) {
  expect(&tok, tok, "#");
  if (!cmp_ident(tok, "include"))
    error_at(tok->str, "this line is not include line");
  expect_kind(&tok, tok, TK_IDENT);

  if (post)
    not_implemented_at(tok->str);

  if (equal(tok, "<")) {
    // ignore include
    warn_at(tok->str, "ignore include");
    consume(&tok, tok, "<");

    while (!equal(tok, ">")) {
      tok = tok->next;
    }

    expect(&tok, tok, ">");
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else if (equal_kind(tok, TK_STR)) {
    Token *file = consume_kind(&tok, tok, TK_STR);
    char *filepath = file->str_literal->str;

    if (!is_included(filepath)) {
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
  if (!cmp_ident(tok, "define"))
    error_at(tok->str, "this line is not define line");
  expect_kind(&tok, tok, TK_IDENT);

  if (post)
    not_implemented_at(tok->str);

  Token *sym_tok = expect_kind(&tok, tok, TK_IDENT);

  DefineList *new_def = calloc(1, sizeof(DefineList));
  new_def->sym_name = sym_tok->str;
  new_def->sym_len = sym_tok->len;

  Token *head = calloc(1, sizeof(Token));
  Token *cur = head;

  while (expand_define(&tok, tok), tok->kind != TK_NEWLINE) {
    cur->next = tok;
    cur = cur->next;

    tok = tok->next;
  }

  new_def->start = head->next;

  new_def->next = define_list;
  define_list = new_def;

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

  DefineList *def = find_define(tok->str, tok->len);
  if (!def) {
    *pre = tok;
    return;
  }

  if (def->start) {
    not_implemented_at(tok->str);
  } else {
    expand_define(pre, tok->next);
  }
}

void process_pragma_line(Token **post, Token **pre, Token *tok) {
  expect(&tok, tok, "#");
  if (!cmp_ident(tok, "pragma"))
    error_at(tok->str, "this line is not pragma line");
  expect_kind(&tok, tok, TK_IDENT);

  if (cmp_ident(tok, "once")) {
    expect_kind(&tok, tok, TK_IDENT);

    PragmaOnceList *pragma = calloc(1, sizeof(PragmaOnceList));
    pragma->filepath = tok->filepath;
    pragma->next = pragma_list;
    pragma_list = pragma;

    expect_kind(&tok, tok, TK_NEWLINE);
    *pre = tok;
  } else {
    not_implemented_at(tok->str);
  }
}

void process_text_line(Token **post, Token **pre, Token *tok) {
  if (equal(tok, "#"))
    error_at(tok->str, "this line is not text line");

  while (expand_define(&tok, tok), tok->kind != TK_NEWLINE) {
    if (post) {
      (*post)->next = tok;
      (*post) = (*post)->next;
    }
    tok = tok->next;
  }

  // skip TK_NEWLINE
  *pre = tok->next;
}

void consume_line(Token **pre, Token *tok) {
  while (tok->kind != TK_NEWLINE)
    tok = tok->next;
  *pre = tok->next;
}

Token *preprocess(Token *tok) {
  Token *head = calloc(1, sizeof(Token));
  Token *cur = head;

  while (!at_eof(tok)) {
    if (equal(tok, "#")) {
      process_macro_group(&cur, &tok, tok);
    } else {
      process_text_line(&cur, &tok, tok);
    }
  }

  cur->next = tok;
  return head->next;
}
