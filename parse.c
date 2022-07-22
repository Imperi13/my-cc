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

static Tree *parse_stmt(Token **rest, Token *tok);
static Tree *parse_label_stmt(Token **rest, Token *tok);
static Tree *parse_compound_stmt(Token **rest, Token *tok);
static Tree *parse_jump_stmt(Token **rest, Token *tok);
static Tree *parse_iteration_stmt(Token **rest, Token *tok);
static Tree *parse_selection_stmt(Token **rest, Token *tok);
static Tree *parse_expr_stmt(Token **rest, Token *tok);
static Tree *parse_expr(Token **rest, Token *tok);
static Tree *parse_assign(Token **rest, Token *tok);
static Tree *parse_conditional(Token **rest, Token *tok);
static Tree *parse_logical_or(Token **rest, Token *tok);
static Tree *parse_logical_and(Token **rest, Token *tok);
static Tree *parse_bit_or(Token **rest, Token *tok);
static Tree *parse_bit_xor(Token **rest, Token *tok);
static Tree *parse_bit_and(Token **rest, Token *tok);
static Tree *parse_equality(Token **rest, Token *tok);
static Tree *parse_relational(Token **rest, Token *tok);
static Tree *parse_shift(Token **rest, Token *tok);
static Tree *parse_add(Token **rest, Token *tok);
static Tree *parse_mul(Token **rest, Token *tok);
static Tree *parse_cast(Token **rest, Token *tok);
static Tree *parse_unary(Token **rest, Token *tok);
static Tree *parse_postfix(Token **rest, Token *tok);
static Tree *parse_primary(Token **rest, Token *tok);

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
  if (equal_kind(tok, TK_INT)) {
    DeclSpec *decl_spec = calloc(1, sizeof(DeclSpec));
    decl_spec->has_int = true;
    consume_kind(rest, tok, TK_INT);
    return decl_spec;
  }
  not_implemented();
  return NULL;
}

Declarator *parse_declarator(Token **rest, Token *tok) {
  Declarator *declarator = calloc(1, sizeof(Declarator));

  // parse ident or nest-declarator
  if (equal_kind(tok, TK_IDENT)) {
    Token *tok = consume_kind(&tok, tok, TK_IDENT);
    declarator->name = tok->str;
    declarator->len = tok->len;
  } else if (equal(tok, "(")) {
    not_implemented();
  } else {
    error("cannot parse declarator");
  }

  // parse type-suffix
  declarator->type_suffix_kind = NONE;
  if (equal(tok, "(")) {
    expect(&tok, tok, "(");
    expect(&tok, tok, ")");
    declarator->type_suffix_kind = FUNC_DECLARATOR;
  } else if (equal(tok, "[")) {
    not_implemented();
  }

  return declarator;
}

Tree *parse_compound_stmt(Token **rest, Token *tok) {
  not_implemented();
  return NULL;
}

Tree *parse_unary(Token **rest, Token *tok) {
  not_implemented();
  return NULL;
}

Tree *parse_postfix(Token **rest, Token *tok) {
  not_implemented();
  return NULL;
}

Tree *parse_primary(Token **rest, Token *tok) {
  Tree *primary = calloc(1, sizeof(Tree));

  if (equal_kind(tok, TK_IDENT)) {
    not_implemented();
  } else if (equal_kind(tok, TK_STR)) {
    not_implemented();
  } else if (equal_kind(tok, TK_NUM)) {
    not_implemented();
  } else if (equal(tok, "(")) {
    not_implemented();
  } else {
    error("cannot parse primary");
  }

  return primary;
}
