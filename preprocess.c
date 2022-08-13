
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "preprocess.h"
#include "tokenize.h"

static void process_macro_group(Token **post, Token **pre, Token *tok);
static void process_if_group(Token **post, Token **pre, Token *tok);
static void process_text_line(Token **post, Token **pre, Token *tok);

Define *define_list;

Define *find_define(char *def_name, int def_len) {
  for (Define *cur = define_list; cur; cur = cur->next)
    if (cur->len == def_len && !memcmp(def_name, cur->next, def_len))
      return cur;
  return NULL;
}

bool cmp_ident(Token *tok, const char *name) {
  return strncmp(tok->str, name, tok->len) == 0;
}

bool is_if_group(Token *tok) {
  return cmp_ident(tok->next, "ifdef") || cmp_ident(tok->next, "ifndef");
}

void process_macro_group(Token **post, Token **pre, Token *tok) {
  if (is_if_group(tok)) {
    process_if_group(post, pre, tok);
  } else if (cmp_ident(tok->next, "include")) {
    // ignore include
    warn_at(tok->str, "ignore include");
    process_text_line(NULL, pre, tok);
  } else {
    not_implemented_at(tok->str);
  }
}

void process_if_group(Token **post, Token **pre, Token *tok) {
  consume(&tok, tok, "#");
  if (cmp_ident(tok, "ifdef")) {
    expect_kind(&tok, tok, TK_IDENT);

    Token *cond_tok = consume_kind(&tok, tok, TK_IDENT);
    bool cond = (find_define(cond_tok->str, cond_tok->len) != NULL);
    expect_kind(&tok, tok, TK_NEWLINE);

    Token *dummy = NULL;
    while (!equal(tok, "#") || !cmp_ident(tok->next, "endif")) {
      if (equal(tok, "#"))
        process_macro_group((cond ? post : NULL), &tok, tok);
      else
        process_text_line((cond ? post : NULL), &tok, tok);
    }

    expect(&tok, tok, "#");
    consume_kind(&tok, tok, TK_IDENT);
    expect_kind(&tok, tok, TK_NEWLINE);

    *pre = tok;
  } else if (cmp_ident(tok, "ifndef")) {
    expect_kind(&tok, tok, TK_IDENT);

    Token *cond_tok = consume_kind(&tok, tok, TK_IDENT);
    bool cond = (find_define(cond_tok->str, cond_tok->len) == NULL);
    expect_kind(&tok, tok, TK_NEWLINE);

    Token *dummy;
    while (!equal(tok, "#")) {
      process_text_line((cond ? post : NULL), &tok, tok);
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

void process_text_line(Token **post, Token **pre, Token *tok) {
  while (tok->kind != TK_NEWLINE) {
    if (post) {
      (*post)->next = tok;
      (*post) = (*post)->next;
    }
    tok = tok->next;
  }

  // skip TK_NEWLINE
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

  cur = new_token(TK_EOF, cur, NULL, 0);
  return head->next;
}
