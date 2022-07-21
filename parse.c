#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "parse.h"
#include "tokenize.h"

static Tree *parse_translation_unit(Token *tok);
static Tree *parse_external_decl(Token **rest, Token *tok);
static DeclSpec *parse_declaration_specs(Token **rest, Token *tok);
static Declarator *parse_declarator(Token **rest, Token *tok);

static Tree *parse_compound_stmt(Token **rest, Token *tok);

Tree *parse_translation_unit(Token *tok) {
  Tree *head = calloc(1, sizeof(Tree));
  Tree *cur = head;

  while (!at_eof(tok)) {
    Tree *ex_decl = parse_external_decl(&tok, tok);
    cur->next = ex_decl;
    cur = ex_decl;
  }
  return head->next;
}

Tree *parse_external_decl(Token **rest, Token *tok) {
  Tree *ex_decl = calloc(1, sizeof(Tree));
  ex_decl->decl_specs = parse_declaration_specs(&tok, tok);
  ex_decl->declarator = parse_declarator(&tok, tok);

  if (equal(tok, "{")) {
    ex_decl->kind = FUNC_DEF;
    ex_decl->func_body = parse_compound_stmt(&tok, tok);
    return ex_decl;
  }

  not_implemented();
  return NULL;
}

DeclSpec *parse_declaration_specs(Token **rest, Token *tok) {
  not_implemented();
  return NULL;
}

Declarator *parse_declarator(Token **rest, Token *tok) {
  not_implemented();
  return NULL;
}

Tree *parse_compound_stmt(Token **rest, Token *tok) {
  not_implemented();
  return NULL;
}
