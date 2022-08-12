
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "preprocess.h"
#include "tokenize.h"

Define *define_list;

Define *find_define(char *def_name, int def_len) {
  for (Define *cur = define_list; cur; cur = cur->next)
    if (cur->len == def_len && !memcmp(def_name, cur->next, def_len))
      return cur;
  return NULL;
}

bool is_if_group(Token *tok) {
  char *name = tok->next->str;
  int len = tok->next->len;
  return strncmp(name, "ifdef", len) == 0;
}

void process_if_group(Token **post, Token **pre, Token *tok) {}

void process_text_line(Token **post, Token **pre, Token *tok) {
  while (tok->kind != TK_NEWLINE) {
    (*post)->next = tok;
    (*post) = (*post)->next;
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
      if (is_if_group(tok)) {
        process_if_group(&cur, &tok, tok);
      }
    } else {
      process_text_line(&cur, &tok, tok);
    }
  }

  cur = new_token(TK_EOF, cur, NULL, 0);
  return head->next;
}
