#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "analyze.h"
#include "error.h"
#include "parse.h"
#include "tokenize.h"

static Tree *parse_external_decl(Token **rest, Token *tok, TypedefScope *state);
static DeclSpec *parse_declaration_specs(Token **rest, Token *tok,
                                         TypedefScope *state);
static Declarator *parse_declarator(Token **rest, Token *tok,
                                    TypedefScope *state);
static Tree *parse_parameter_type_list(Token **rest, Token *tok,
                                       TypedefScope *state);

static Tree *parse_stmt(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_label_stmt(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_compound_stmt(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_jump_stmt(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_iteration_stmt(Token **rest, Token *tok,
                                  TypedefScope *state);
static Tree *parse_selection_stmt(Token **rest, Token *tok,
                                  TypedefScope *state);
static Tree *parse_expr_stmt(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_expr(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_assign(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_conditional(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_logical_or(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_logical_and(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_bit_or(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_bit_xor(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_bit_and(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_equality(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_relational(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_shift(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_add(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_mul(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_cast(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_unary(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_postfix(Token **rest, Token *tok, TypedefScope *state);
static Tree *parse_primary(Token **rest, Token *tok, TypedefScope *state);

static bool is_label_stmt(Token *tok);
static bool is_selection_stmt(Token *tok);
static bool is_iteration_stmt(Token *tok);
static bool is_jump_stmt(Token *tok);

static bool is_declaration_specs(Token *tok, TypedefScope *state);

Tree *new_binary_node(TreeKind kind, Tree *lhs, Tree *rhs) {
  Tree *node = calloc(1, sizeof(Tree));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

Tree *parse_translation_unit(Token *tok) {
  Tree *head = calloc(1, sizeof(Tree));
  Tree *cur = head;

  TypedefScope *state = calloc(1, sizeof(TypedefScope));

  while (!at_eof(tok)) {
    Tree *ex_decl = parse_external_decl(&tok, tok, state);
    cur->next = ex_decl;
    cur = ex_decl;
  }

  return head->next;
}

Tree *parse_external_decl(Token **rest, Token *tok, TypedefScope *state) {
  Tree *ex_decl = calloc(1, sizeof(Tree));
  ex_decl->decl_specs = parse_declaration_specs(&tok, tok, state);

  if (equal(tok, ";")) {
    ex_decl->kind = DECLARATION;
    not_implemented_at(tok->str);
  }

  ex_decl->declarator = parse_declarator(&tok, tok, state);

  if (equal(tok, "{")) {
    ex_decl->kind = FUNC_DEF;
    ex_decl->func_body = parse_compound_stmt(&tok, tok, state);
    *rest = tok;
    return ex_decl;
  }

  // TODO initialize
  // TODO multiple declarator

  ex_decl->kind = DECLARATION;

  expect(&tok, tok, ";");
  *rest = tok;
  return ex_decl;
}

bool is_declaration_specs(Token *tok, TypedefScope *state) {
  return equal_kind(tok, TK_INT) || equal_kind(tok, TK_CHAR);
}

DeclSpec *parse_declaration_specs(Token **rest, Token *tok,
                                  TypedefScope *state) {
  if (equal_kind(tok, TK_INT)) {
    DeclSpec *decl_spec = calloc(1, sizeof(DeclSpec));
    decl_spec->has_int = true;
    consume_kind(rest, tok, TK_INT);
    return decl_spec;
  } else if (equal_kind(tok, TK_CHAR)) {
    DeclSpec *decl_spec = calloc(1, sizeof(DeclSpec));
    decl_spec->has_char = true;
    consume_kind(rest, tok, TK_CHAR);
    return decl_spec;
  }
  not_implemented_at(tok->str);
  return NULL;
}

Declarator *parse_declarator(Token **rest, Token *tok, TypedefScope *state) {
  Declarator *declarator = calloc(1, sizeof(Declarator));

  Pointer **cur = &declarator->pointer;
  while (equal(tok, "*")) {
    consume(&tok, tok, "*");

    *cur = calloc(1, sizeof(Pointer));
    cur = &(*cur)->nest;
  }

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
    declarator->type_suffix_kind = FUNC_DECLARATOR;
    if (!consume(&tok, tok, ")")) {
      declarator->args = parse_parameter_type_list(&tok, tok, state);
      consume(&tok, tok, ")");
    }
  } else if (equal(tok, "[")) {
    declarator->type_suffix_kind = ARRAY_DECLARATOR;
    while (consume(&tok, tok, "[")) {
      ArrayDeclarator *arr_decl = calloc(1, sizeof(ArrayDeclarator));
      arr_decl->size = parse_expr(&tok, tok, state);
      consume(&tok, tok, "]");

      arr_decl->next = declarator->arr_decl;
      declarator->arr_decl = arr_decl;
    }
  }

  *rest = tok;
  return declarator;
}

Tree *parse_parameter_type_list(Token **rest, Token *tok, TypedefScope *state) {
  Tree *head = calloc(1, sizeof(Tree));
  head->kind = DECLARATION;
  head->decl_specs = parse_declaration_specs(&tok, tok, state);
  head->declarator = parse_declarator(&tok, tok, state);
  Tree *cur = head;

  while (consume(&tok, tok, ",")) {
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = DECLARATION;
    node->decl_specs = parse_declaration_specs(&tok, tok, state);
    node->declarator = parse_declarator(&tok, tok, state);

    cur->next = node;
    cur = node;
  }

  *rest = tok;
  return head;
}

Tree *parse_stmt(Token **rest, Token *tok, TypedefScope *state) {
  Tree *node;

  if (equal(tok, "{")) {
    node = parse_compound_stmt(&tok, tok, state);
  } else if (is_label_stmt(tok)) {
    node = parse_label_stmt(&tok, tok, state);
  } else if (is_selection_stmt(tok)) {
    node = parse_selection_stmt(&tok, tok, state);
  } else if (is_iteration_stmt(tok)) {
    node = parse_iteration_stmt(&tok, tok, state);
  } else if (is_jump_stmt(tok)) {
    node = parse_jump_stmt(&tok, tok, state);
  } else {
    node = parse_expr_stmt(&tok, tok, state);
  }
  *rest = tok;
  return node;
}

bool is_label_stmt(Token *tok) {
  return (equal_kind(tok, TK_IDENT) && equal(tok->next, ":")) ||
         equal_kind(tok, TK_DEFAULT) || equal_kind(tok, TK_CASE);
}

Tree *parse_label_stmt(Token **rest, Token *tok, TypedefScope *state) {
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

Tree *parse_compound_stmt(Token **rest, Token *tok, TypedefScope *state) {
  expect(&tok, tok, "{");
  Tree *head = calloc(1, sizeof(Tree));
  Tree *cur = head;
  while (!consume(&tok, tok, "}")) {
    if (is_declaration_specs(tok, state)) {
      cur->next = parse_external_decl(&tok, tok, state);
      cur = cur->next;
    } else {
      cur->next = parse_stmt(&tok, tok, state);
      cur = cur->next;
    }
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

Tree *parse_jump_stmt(Token **rest, Token *tok, TypedefScope *state) {
  Tree *node = calloc(1, sizeof(Tree));
  if (equal_kind(tok, TK_RETURN)) {

    consume_kind(&tok, tok, TK_RETURN);
    node->kind = RETURN;
    if (equal(tok, ";")) {
      consume(&tok, tok, ";");
    } else {
      node->lhs = parse_expr(&tok, tok, state);
      expect(&tok, tok, ";");
    }

  } else if (equal_kind(tok, TK_BREAK)) {
    consume_kind(&tok, tok, TK_BREAK);
    node->kind = BREAK;
    expect(&tok, tok, ";");
  } else if (equal_kind(tok, TK_CONTINUE)) {
    consume_kind(&tok, tok, TK_CONTINUE);
    node->kind = CONTINUE;
    expect(&tok, tok, ";");
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

Tree *parse_iteration_stmt(Token **rest, Token *tok, TypedefScope *state) {
  Tree *node = NULL;
  if (equal_kind(tok, TK_WHILE)) {
    consume_kind(&tok, tok, TK_WHILE);

    node = calloc(1, sizeof(Tree));
    node->kind = WHILE;

    expect(&tok, tok, "(");
    node->cond = parse_expr(&tok, tok, state);
    expect(&tok, tok, ")");

    node->lhs = parse_stmt(&tok, tok, state);
  } else if (equal_kind(tok, TK_DO)) {
    consume_kind(&tok, tok, TK_DO);

    node = calloc(1, sizeof(Tree));
    node->kind = DO_WHILE;

    node->lhs = parse_stmt(&tok, tok, state);

    expect_kind(&tok, tok, TK_WHILE);
    expect(&tok, tok, "(");

    node->cond = parse_expr(&tok, tok, state);

    expect(&tok, tok, ")");
    expect(&tok, tok, ";");
  } else if (equal_kind(tok, TK_FOR)) {
    consume_kind(&tok, tok, TK_FOR);

    node = calloc(1, sizeof(Tree));
    node->kind = FOR;

    consume(&tok, tok, "(");

    if (!equal(tok, ";"))
      node->for_init = parse_expr(&tok, tok, state);

    consume(&tok, tok, ";");

    if (!equal(tok, ";")) {
      node->cond = parse_expr(&tok, tok, state);
    } else {
      node->cond = calloc(1, sizeof(Tree));
      node->cond->kind = NUM;
      node->cond->num = 1;
    }

    consume(&tok, tok, ";");

    if (!equal(tok, ")"))
      node->for_update = parse_expr(&tok, tok, state);

    consume(&tok, tok, ")");

    node->lhs = parse_stmt(&tok, tok, state);

  } else {
    error("cannot parse selection_stmt");
  }
  *rest = tok;
  return node;
}

bool is_selection_stmt(Token *tok) {
  return equal_kind(tok, TK_IF) || equal_kind(tok, TK_SWITCH);
}

Tree *parse_selection_stmt(Token **rest, Token *tok, TypedefScope *state) {
  Tree *node = NULL;
  if (equal_kind(tok, TK_IF)) {
    consume_kind(&tok, tok, TK_IF);
    expect(&tok, tok, "(");
    node = calloc(1, sizeof(Tree));
    node->kind = IF;
    node->cond = parse_expr(&tok, tok, state);
    expect(&tok, tok, ")");

    node->lhs = parse_stmt(&tok, tok, state);

    if (consume_kind(&tok, tok, TK_ELSE))
      node->rhs = parse_stmt(&tok, tok, state);

  } else if (equal_kind(tok, TK_SWITCH)) {
    not_implemented_at(tok->str);
  } else {
    error("cannot parse selection_stmt");
  }

  *rest = tok;
  return node;
}

Tree *parse_expr_stmt(Token **rest, Token *tok, TypedefScope *state) {
  if (equal(tok, ";")) {
    not_implemented_at(tok->str);
  }

  Tree *node = parse_expr(&tok, tok, state);
  expect(rest, tok, ";");
  return node;
}

Tree *parse_expr(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_assign(&tok, tok, state);
  for (;;) {
    if (equal(tok, ",")) {
      consume(&tok, tok, ",");
      Tree *rhs = parse_assign(&tok, tok, state);
      lhs = new_binary_node(COMMA, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_assign(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_conditional(&tok, tok, state);

  if (equal(tok, "=")) {
    consume(&tok, tok, "=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(ASSIGN, lhs, rhs);
  } else if (equal(tok, "+=")) {
    consume(&tok, tok, "+=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(ADD_ASSIGN, lhs, rhs);
  } else if (equal(tok, "-=")) {
    consume(&tok, tok, "-=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(SUB_ASSIGN, lhs, rhs);
  } else if (equal(tok, "*=")) {
    consume(&tok, tok, "*=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(MUL_ASSIGN, lhs, rhs);
  } else if (equal(tok, "/=")) {
    consume(&tok, tok, "/=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(DIV_ASSIGN, lhs, rhs);
  } else if (equal(tok, "%=")) {
    consume(&tok, tok, "%=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(MOD_ASSIGN, lhs, rhs);
  } else if (equal(tok, "&=")) {
    consume(&tok, tok, "&=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(AND_ASSIGN, lhs, rhs);
  } else if (equal(tok, "|=")) {
    consume(&tok, tok, "|=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(OR_ASSIGN, lhs, rhs);
  } else if (equal(tok, "^=")) {
    consume(&tok, tok, "^=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(XOR_ASSIGN, lhs, rhs);
  } else if (equal(tok, "<<=")) {
    consume(&tok, tok, "<<=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(LSHIFT_ASSIGN, lhs, rhs);
  } else if (equal(tok, ">>=")) {
    consume(&tok, tok, ">>=");
    Tree *rhs = parse_assign(&tok, tok, state);
    lhs = new_binary_node(RSHIFT_ASSIGN, lhs, rhs);
  }

  *rest = tok;
  return lhs;
}

Tree *parse_conditional(Token **rest, Token *tok, TypedefScope *state) {
  Tree *cond = parse_logical_or(&tok, tok, state);
  if (equal(tok, "?")) {
    consume(&tok, tok, "?");
    Tree *lhs = parse_expr(&tok, tok, state);
    expect(&tok, tok, ":");
    Tree *rhs = parse_conditional(&tok, tok, state);

    Tree *node = calloc(1, sizeof(Tree));
    node->kind = CONDITIONAL;
    node->cond = cond;
    node->lhs = lhs;
    node->rhs = rhs;

    *rest = tok;
    return node;
  }
  *rest = tok;
  return cond;
}

Tree *parse_logical_or(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_logical_and(&tok, tok, state);
  for (;;) {
    if (equal(tok, "||")) {
      consume(&tok, tok, "||");
      Tree *rhs = parse_logical_and(&tok, tok, state);
      lhs = new_binary_node(LOGICAL_OR, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_logical_and(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_bit_or(&tok, tok, state);
  for (;;) {
    if (equal(tok, "&&")) {
      consume(&tok, tok, "&&");
      Tree *rhs = parse_bit_or(&tok, tok, state);
      lhs = new_binary_node(LOGICAL_AND, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_bit_or(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_bit_xor(&tok, tok, state);
  for (;;) {
    if (equal(tok, "|")) {
      consume(&tok, tok, "|");
      Tree *rhs = parse_bit_xor(&tok, tok, state);
      lhs = new_binary_node(BIT_OR, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_bit_xor(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_bit_and(&tok, tok, state);
  for (;;) {
    if (equal(tok, "^")) {
      consume(&tok, tok, "^");
      Tree *rhs = parse_bit_and(&tok, tok, state);
      lhs = new_binary_node(BIT_XOR, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_bit_and(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_equality(&tok, tok, state);
  for (;;) {
    if (equal(tok, "&")) {
      consume(&tok, tok, "&");
      Tree *rhs = parse_equality(&tok, tok, state);
      lhs = new_binary_node(BIT_AND, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_equality(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_relational(&tok, tok, state);

  for (;;) {
    if (equal(tok, "==")) {
      consume(&tok, tok, "==");
      Tree *rhs = parse_relational(&tok, tok, state);
      lhs = new_binary_node(EQUAL, lhs, rhs);
    } else if (equal(tok, "!=")) {
      consume(&tok, tok, "!=");
      Tree *rhs = parse_relational(&tok, tok, state);
      lhs = new_binary_node(NOT_EQUAL, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_relational(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_shift(&tok, tok, state);

  for (;;) {
    if (equal(tok, "<")) {
      consume(&tok, tok, "<");
      Tree *rhs = parse_shift(&tok, tok, state);
      lhs = new_binary_node(SMALLER, lhs, rhs);
    } else if (equal(tok, "<=")) {
      consume(&tok, tok, "<=");
      Tree *rhs = parse_shift(&tok, tok, state);
      lhs = new_binary_node(SMALLER_EQUAL, lhs, rhs);
    } else if (equal(tok, ">")) {
      consume(&tok, tok, ">");
      Tree *rhs = parse_shift(&tok, tok, state);
      lhs = new_binary_node(GREATER, lhs, rhs);
    } else if (equal(tok, ">=")) {
      consume(&tok, tok, ">=");
      Tree *rhs = parse_shift(&tok, tok, state);
      lhs = new_binary_node(GREATER_EQUAL, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_shift(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_add(&tok, tok, state);

  for (;;) {
    if (equal(tok, "<<")) {
      consume(&tok, tok, "<<");
      Tree *rhs = parse_add(&tok, tok, state);
      lhs = new_binary_node(LSHIFT, lhs, rhs);
    } else if (equal(tok, ">>")) {
      consume(&tok, tok, ">>");
      Tree *rhs = parse_add(&tok, tok, state);
      lhs = new_binary_node(RSHIFT, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_add(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_mul(&tok, tok, state);

  for (;;) {
    if (equal(tok, "+")) {
      consume(&tok, tok, "+");
      Tree *rhs = parse_mul(&tok, tok, state);
      lhs = new_binary_node(ADD, lhs, rhs);
    } else if (equal(tok, "-")) {
      consume(&tok, tok, "-");
      Tree *rhs = parse_mul(&tok, tok, state);
      lhs = new_binary_node(SUB, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_mul(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_cast(&tok, tok, state);

  for (;;) {
    if (equal(tok, "*")) {
      consume(&tok, tok, "*");
      Tree *rhs = parse_cast(&tok, tok, state);
      lhs = new_binary_node(MUL, lhs, rhs);
    } else if (equal(tok, "/")) {
      consume(&tok, tok, "/");
      Tree *rhs = parse_cast(&tok, tok, state);
      lhs = new_binary_node(DIV, lhs, rhs);
    } else if (equal(tok, "%")) {
      consume(&tok, tok, "%");
      Tree *rhs = parse_cast(&tok, tok, state);
      lhs = new_binary_node(MOD, lhs, rhs);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_cast(Token **rest, Token *tok, TypedefScope *state) {
  Tree *node = parse_unary(rest, tok, state);
  return node;
}

Tree *parse_unary(Token **rest, Token *tok, TypedefScope *state) {
  if (equal_kind(tok, TK_SIZEOF)) {
    not_implemented_at(tok->str);
    return NULL;
  }

  if (equal_kind(tok, TK_ALIGNOF)) {
    not_implemented_at(tok->str);
    return NULL;
  }
  if (equal(tok, "++")) {
    consume(&tok, tok, "++");
    Tree *lhs = parse_cast(&tok, tok, state);
    Tree *rhs = calloc(1, sizeof(Tree));
    rhs->kind = NUM;
    rhs->num = 1;

    *rest = tok;
    return new_binary_node(ADD_ASSIGN, lhs, rhs);
  }

  if (equal(tok, "--")) {
    consume(&tok, tok, "--");
    Tree *lhs = parse_cast(&tok, tok, state);
    Tree *rhs = calloc(1, sizeof(Tree));
    rhs->kind = NUM;
    rhs->num = 1;

    *rest = tok;
    return new_binary_node(SUB_ASSIGN, lhs, rhs);
  }

  if (equal(tok, "+")) {
    consume(&tok, tok, "+");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = PLUS;
    node->lhs = parse_cast(&tok, tok, state);
    *rest = tok;
    return node;
  }

  if (equal(tok, "-")) {
    consume(&tok, tok, "-");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = MINUS;
    node->lhs = parse_cast(&tok, tok, state);
    *rest = tok;
    return node;
  }

  if (equal(tok, "&")) {
    consume(&tok, tok, "&");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = ADDR;
    node->lhs = parse_cast(&tok, tok, state);
    *rest = tok;
    return node;
  }

  if (equal(tok, "*")) {
    consume(&tok, tok, "*");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = DEREF;
    node->lhs = parse_cast(&tok, tok, state);
    *rest = tok;
    return node;
  }

  if (equal(tok, "!")) {
    consume(&tok, tok, "!");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = LOGICAL_NOT;
    node->lhs = parse_cast(&tok, tok, state);
    *rest = tok;
    return node;
  }

  if (equal(tok, "~")) {
    consume(&tok, tok, "~");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = BIT_NOT;
    node->lhs = parse_cast(&tok, tok, state);
    *rest = tok;
    return node;
  }

  Tree *lhs = parse_postfix(rest, tok, state);
  return lhs;
}

Tree *parse_postfix(Token **rest, Token *tok, TypedefScope *state) {
  Tree *lhs = parse_primary(&tok, tok, state);

  for (;;) {
    if (equal(tok, "[")) {
      consume(&tok, tok, "[");
      Tree *rhs = parse_expr(&tok, tok, state);
      consume(&tok, tok, "]");

      Tree *add_node = new_binary_node(ADD, lhs, rhs);
      lhs = new_binary_node(DEREF, add_node, NULL);
    } else if (equal(tok, "(")) {
      Tree *node = calloc(1, sizeof(Tree));
      node->kind = FUNC_CALL;
      node->lhs = lhs;
      consume(&tok, tok, "(");

      while (!consume(&tok, tok, ")")) {
        Tree *arg = parse_assign(&tok, tok, state);
        arg->next = node->call_args;
        node->call_args = arg;
        consume(&tok, tok, ",");
      }

      lhs = node;
    } else if (equal(tok, ".")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "->")) {
      not_implemented_at(tok->str);
    } else if (equal(tok, "++")) {
      consume(&tok, tok, "++");
      lhs = new_binary_node(POST_INCREMENT, lhs, NULL);
    } else if (equal(tok, "--")) {
      consume(&tok, tok, "--");
      lhs = new_binary_node(POST_DECREMENT, lhs, NULL);
    } else {
      *rest = tok;
      return lhs;
    }
  }
}

Tree *parse_primary(Token **rest, Token *tok, TypedefScope *state) {
  Tree *primary = calloc(1, sizeof(Tree));

  if (equal_kind(tok, TK_NUM)) {
    Token *num_tok = consume_kind(&tok, tok, TK_NUM);
    primary->kind = NUM;
    primary->num = num_tok->val;
  } else if (equal_kind(tok, TK_STR)) {
    StrLiteral *str_literal = consume_kind(&tok, tok, TK_STR)->str_literal;

    primary->kind = STR;
    primary->str_literal = str_literal;
  } else if (equal_kind(tok, TK_IDENT)) {
    Token *ident_tok = consume_kind(&tok, tok, TK_IDENT);
    primary->kind = VAR;
    primary->var_name = ident_tok->str;
    primary->var_len = ident_tok->len;
  } else if (equal(tok, "(")) {
    consume(&tok, tok, "(");
    primary = parse_expr(&tok, tok, state);
    expect(&tok, tok, ")");
  } else {
    error("cannot parse primary");
  }

  *rest = tok;
  return primary;
}
