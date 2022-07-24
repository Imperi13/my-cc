#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "parse.h"
#include "tokenize.h"

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

static bool is_label_stmt(Token *tok);
static bool is_selection_stmt(Token *tok);
static bool is_iteration_stmt(Token *tok);
static bool is_jump_stmt(Token *tok);

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
    *rest = tok;
    return ex_decl;
  }

  not_implemented_at(tok->str);
  return NULL;
}

DeclSpec *parse_declaration_specs(Token **rest, Token *tok) {
  if (equal_kind(tok, TK_INT)) {
    DeclSpec *decl_spec = calloc(1, sizeof(DeclSpec));
    decl_spec->has_int = true;
    consume_kind(rest, tok, TK_INT);
    return decl_spec;
  }
  not_implemented_at(tok->str);
  return NULL;
}

Declarator *parse_declarator(Token **rest, Token *tok) {
  Declarator *declarator = calloc(1, sizeof(Declarator));

  // parse ident or nest-declarator
  if (equal_kind(tok, TK_IDENT)) {
    Token *decl_name = consume_kind(&tok, tok, TK_IDENT);
    declarator->name = decl_name->str;
    declarator->len = decl_name->len;
  } else if (equal(tok, "(")) {
    not_implemented_at(tok->str);
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
    not_implemented_at(tok->str);
  }

  *rest = tok;
  return declarator;
}

Tree *parse_stmt(Token **rest, Token *tok) {
  Tree *node;

  if (equal(tok, "{")) {
    node = parse_compound_stmt(&tok, tok);
  } else if (is_label_stmt(tok)) {
    node = parse_label_stmt(&tok, tok);
  } else if (is_selection_stmt(tok)) {
    node = parse_selection_stmt(&tok, tok);
  } else if (is_iteration_stmt(tok)) {
    node = parse_iteration_stmt(&tok, tok);
  } else if (is_jump_stmt(tok)) {
    node = parse_jump_stmt(&tok, tok);
  } else {
    node = parse_expr_stmt(&tok, tok);
  }
  *rest = tok;
  return node;
}

bool is_label_stmt(Token *tok) {
  return (equal_kind(tok, TK_IDENT) && equal(tok->next, ":")) ||
         equal_kind(tok, TK_DEFAULT) || equal_kind(tok, TK_CASE);
}

Tree *parse_label_stmt(Token **rest, Token *tok) {
  Tree *node = NULL;
  if (equal_kind(tok, TK_IDENT) && equal(tok->next, ":")) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_CASE)) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_DEFAULT)) {
    not_implemented_at(tok->str);
  }
  return node;
}

Tree *parse_compound_stmt(Token **rest, Token *tok) {
  expect(&tok, tok, "{");
  Tree *head = calloc(1, sizeof(Tree));
  Tree *cur = head;
  while (!consume(&tok, tok, "}")) {
    cur->next = parse_stmt(&tok, tok);
    cur = cur->next;
  }
  *rest = tok;

  Tree *node = calloc(1, sizeof(Tree));
  node->kind = COMPOUND_STMT;
  node->stmts = head->next;
  return node;
}

bool is_jump_stmt(Token *tok) {
  return equal_kind(tok, TK_RETURN) || equal_kind(tok, TK_BREAK) ||
         equal_kind(tok, TK_CONTINUE);
}

Tree *parse_jump_stmt(Token **rest, Token *tok) {
  Tree *node = calloc(1, sizeof(Tree));
  if (equal_kind(tok, TK_RETURN)) {

    consume_kind(&tok, tok, TK_RETURN);
    node->kind = RETURN;
    if (equal(tok, ";")) {
      consume(&tok, tok, ";");
    } else {
      node->lhs = parse_expr(&tok, tok);
      expect(&tok, tok, ";");
    }

  } else if (equal_kind(tok, TK_BREAK)) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_CONTINUE)) {
    not_implemented_at(tok->str);
  } else {
    error("cannot parse selection_stmt");
  }

  *rest = tok;
  return node;
}

bool is_iteration_stmt(Token *tok) {
  return equal_kind(tok, TK_WHILE) || equal_kind(tok, TK_DO) ||
         equal_kind(tok, TK_FOR);
}

Tree *parse_iteration_stmt(Token **rest, Token *tok) {
  Tree *node = NULL;
  if (equal_kind(tok, TK_WHILE)) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_DO)) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_FOR)) {
    not_implemented_at(tok->str);
  } else {
    error("cannot parse selection_stmt");
  }
  return node;
}

bool is_selection_stmt(Token *tok) {
  return equal_kind(tok, TK_IF) || equal_kind(tok, TK_SWITCH);
}

Tree *parse_selection_stmt(Token **rest, Token *tok) {
  Tree *node = NULL;
  if (equal_kind(tok, TK_IF)) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_SWITCH)) {
    not_implemented_at(tok->str);
  } else {
    error("cannot parse selection_stmt");
  }
  return node;
}

Tree *parse_expr_stmt(Token **rest, Token *tok) {
  if (equal(tok, ";")) {
    not_implemented_at(tok->str);
  }

  Tree *node = parse_expr(&tok, tok);
  expect(rest, tok, ";");
  return node;
}

Tree *parse_expr(Token **rest, Token *tok) {
  Tree *lhs = parse_assign(&tok, tok);
  for (;;) {
    if (equal(tok, ",")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_assign(Token **rest, Token *tok) {
  Tree *lhs = parse_conditional(&tok, tok);

  if (equal(tok, "=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "+=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "-=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "*=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "/=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "%=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "&=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "|=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "^=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "<<=")) {
    not_implemented_at(tok->str);
  } else if (equal(tok, ">>=")) {
    not_implemented_at(tok->str);
  }

  *rest = tok;
  return lhs;
}

Tree *parse_conditional(Token **rest, Token *tok) {
  Tree *cond = parse_logical_or(&tok, tok);
  if (equal(tok, "?")) {
    not_implemented_at(tok->str);
  }
  *rest = tok;
  return cond;
}

Tree *parse_logical_or(Token **rest, Token *tok) {
  Tree *lhs = parse_logical_and(&tok, tok);
  for (;;) {
    if (equal(tok, "||")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_logical_and(Token **rest, Token *tok) {
  Tree *lhs = parse_bit_or(&tok, tok);
  for (;;) {
    if (equal(tok, "&&")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_bit_or(Token **rest, Token *tok) {
  Tree *lhs = parse_bit_xor(&tok, tok);
  for (;;) {
    if (equal(tok, "|")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_bit_xor(Token **rest, Token *tok) {
  Tree *lhs = parse_bit_and(&tok, tok);
  for (;;) {
    if (equal(tok, "^")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_bit_and(Token **rest, Token *tok) {
  Tree *lhs = parse_equality(&tok, tok);
  for (;;) {
    if (equal(tok, "&")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_equality(Token **rest, Token *tok) {
  Tree *lhs = parse_relational(&tok, tok);

  for (;;) {
    if (equal(tok, "==")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "!=")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_relational(Token **rest, Token *tok) {
  Tree *lhs = parse_shift(&tok, tok);

  for (;;) {
    if (equal(tok, "<")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "<=")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, ">")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, ">=")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_shift(Token **rest, Token *tok) {
  Tree *lhs = parse_add(&tok, tok);

  for (;;) {
    if (equal(tok, "<<")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, ">>")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_add(Token **rest, Token *tok) {
  Tree *lhs = parse_mul(&tok, tok);

  for (;;) {
    if (equal(tok, "+")) {
      consume(&tok, tok, "+");
      Tree *add_node = calloc(1, sizeof(Tree));
      add_node->kind = ADD;
      add_node->lhs = lhs;
      add_node->rhs = parse_mul(&tok, tok);

      lhs = add_node;
    } else if (equal(tok, "-")) {
      consume(&tok, tok, "-");
      Tree *sub_node = calloc(1, sizeof(Tree));
      sub_node->kind = SUB;
      sub_node->lhs = lhs;
      sub_node->rhs = parse_mul(&tok, tok);

      lhs = sub_node;
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_mul(Token **rest, Token *tok) {
  Tree *lhs = parse_cast(&tok, tok);

  for (;;) {
    if (equal(tok, "*")) {
      consume(&tok, tok, "*");
      Tree *mul_node = calloc(1, sizeof(Tree));
      mul_node->kind = MUL;
      mul_node->lhs = lhs;
      mul_node->rhs = parse_cast(&tok, tok);

      lhs = mul_node;
    } else if (equal(tok, "/")) {
      consume(&tok, tok, "/");
      Tree *div_node = calloc(1, sizeof(Tree));
      div_node->kind = DIV;
      div_node->lhs = lhs;
      div_node->rhs = parse_cast(&tok, tok);

      lhs = div_node;
    } else if (equal(tok, "%")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_cast(Token **rest, Token *tok) {
  Tree *node = parse_unary(rest, tok);
  return node;
}

Tree *parse_unary(Token **rest, Token *tok) {
  if (equal_kind(tok, TK_SIZEOF)) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal_kind(tok, TK_ALIGNOF)) {
    not_implemented_at(tok->str);
    return NULL;
  }
  if (equal(tok, "++")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "--")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "+")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "-")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "*")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "&")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "!")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal(tok, "~")) {
    not_implemented_at(tok->str);
    return NULL;
  }

  Tree *lhs = parse_postfix(rest, tok);
  return lhs;
}

Tree *parse_postfix(Token **rest, Token *tok) {
  Tree *lhs = parse_primary(&tok, tok);

  for (;;) {
    if (equal(tok, "[")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "(")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, ".")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "->")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "++")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "--")) {
      not_implemented_at(tok->str);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_primary(Token **rest, Token *tok) {
  Tree *primary = calloc(1, sizeof(Tree));

  if (equal_kind(tok, TK_NUM)) {
    Token *num_tok = consume_kind(&tok, tok, TK_NUM);
    primary->kind = NUM;
    primary->num = num_tok->val;
  } else if (equal_kind(tok, TK_STR)) {
    not_implemented_at(tok->str);
  } else if (equal_kind(tok, TK_IDENT)) {
    not_implemented_at(tok->str);
  } else if (equal(tok, "(")) {
    consume(&tok, tok, "(");
    primary = parse_expr(&tok, tok);
    expect(&tok, tok, ")");
  } else {
    error("cannot parse primary");
  }

  *rest = tok;
  return primary;
}
