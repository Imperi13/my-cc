
#include <stdlib.h>

#include "error.h"
#include "preprocess.h"
#include "tokenize.h"

Token *preprocess(Token *tok) {
  Token *head = calloc(1, sizeof(Token));
  Token *cur = head;

  while (!at_eof(tok)) {
    if (equal(tok, "#")) {
      not_implemented_at(tok->str);
    } else {
      while (tok->kind != TK_NEWLINE) {
        cur->next = tok;
        cur = cur->next;
        tok = tok->next;
      }

      // skip TK_NEWLINE
      tok = tok->next;
    }
  }

  cur = new_token(TK_EOF,cur,NULL,0); 
  return head->next;
}
