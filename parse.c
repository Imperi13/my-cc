#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "error.h"
#include "parse.h"
#include "tokenize.h"

typedef struct PrimitiveTypeToken PrimitiveTypeToken;
struct PrimitiveTypeToken {
  int void_count;
  int bool_count;
  int short_count;
  int char_count;
  int int_count;
  int long_count;
  int signed_count;
  int unsigned_count;
  int float_count;
  int double_count;
};

static Tree *parse_external_decl(Token **rest, Token *tok, Analyze *state,
                                 bool allow_function);
static Tree *parse_initialize_list(Token **rest, Token *tok, Analyze *state);
static Tree *parse_struct_declaration(Token **rest, Token *tok, Analyze *state);
static DeclSpec *parse_decl_specs(Token **rest, Token *tok, Analyze *state);

static void parse_primitive_type_spec(Token **rest, Token *tok,
                                      PrimitiveTypeToken *primitive_type_token);
static StructSpec *parse_struct_spec(Token **rest, Token *tok, Analyze *state);
static UnionSpec *parse_union_spec(Token **rest, Token *tok, Analyze *state);
static EnumSpec *parse_enum_spec(Token **rest, Token *tok, Analyze *state);

static void
set_primitive_type_spec_kind(DeclSpec *decl_spec,
                             PrimitiveTypeToken *primitive_type_token);

static Declarator *parse_declarator(Token **rest, Token *tok, Analyze *state);
static Tree *parse_parameter_type_list(Token **rest, Token *tok,
                                       Analyze *state);
static Tree *parse_type_name(Token **rest, Token *tok, Analyze *state);
static Declarator *parse_abstract_declarator(Token **rest, Token *tok,
                                             Analyze *state);

static bool is_decl_specs(Token *tok, Analyze *state);
static bool is_declaration(Token *tok, Analyze *state);
static bool is_declarator(Token *tok, Analyze *state);

static Tree *parse_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_label_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_compound_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_jump_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_iteration_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_selection_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_expr_stmt(Token **rest, Token *tok, Analyze *state);
static Tree *parse_expr(Token **rest, Token *tok, Analyze *state);
static Tree *parse_assign(Token **rest, Token *tok, Analyze *state);

static Tree *parse_constant_expr(Token **rest, Token *tok, Analyze *state);
static Tree *parse_conditional(Token **rest, Token *tok, Analyze *state);
static Tree *parse_logical_or(Token **rest, Token *tok, Analyze *state);
static Tree *parse_logical_and(Token **rest, Token *tok, Analyze *state);
static Tree *parse_bit_or(Token **rest, Token *tok, Analyze *state);
static Tree *parse_bit_xor(Token **rest, Token *tok, Analyze *state);
static Tree *parse_bit_and(Token **rest, Token *tok, Analyze *state);
static Tree *parse_equality(Token **rest, Token *tok, Analyze *state);
static Tree *parse_relational(Token **rest, Token *tok, Analyze *state);
static Tree *parse_shift(Token **rest, Token *tok, Analyze *state);
static Tree *parse_add(Token **rest, Token *tok, Analyze *state);
static Tree *parse_mul(Token **rest, Token *tok, Analyze *state);
static Tree *parse_cast(Token **rest, Token *tok, Analyze *state);
static Tree *parse_unary(Token **rest, Token *tok, Analyze *state);
static Tree *parse_postfix(Token **rest, Token *tok, Analyze *state);
static Tree *parse_primary(Token **rest, Token *tok, Analyze *state);

static Tree *parse_builtin(Token **rest, Token *tok, Analyze *state);

static bool is_label_stmt(Token *tok);
static bool is_selection_stmt(Token *tok);
static bool is_iteration_stmt(Token *tok);
static bool is_jump_stmt(Token *tok);

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

  Analyze *state = new_analyze_state();

  builtin_type_init(state);

  while (!at_eof(tok)) {
    Tree *ex_decl = parse_external_decl(&tok, tok, state, true);
    cur->next = ex_decl;
    cur = ex_decl;
  }

  return head->next;
}

Tree *parse_external_decl(Token **rest, Token *tok, Analyze *state,
                          bool allow_function) {
  Tree *ex_decl = calloc(1, sizeof(Tree));
  ex_decl->decl_specs = parse_decl_specs(&tok, tok, state);

  if (equal(tok, ";")) {
    consume(rest, tok, ";");

    if (ex_decl->decl_specs->has_typedef)
      error_token(tok, "typedef needs def_name");
    ex_decl->kind = DECLARATION;
    return ex_decl;
  }

  ex_decl->declarator = parse_declarator(&tok, tok, state);

  if (equal(tok, "{")) {
    if (!allow_function)
      error("not allow func-def");
    if (ex_decl->decl_specs->has_typedef)
      error_token(tok, "cannot typedef for FUNC_DEF");
    ex_decl->kind = FUNC_DEF;
    ex_decl->func_body = parse_compound_stmt(&tok, tok, state);
    *rest = tok;
    return ex_decl;
  }

  // parse DECLARATION
  ex_decl->kind = DECLARATION;

  if (equal(tok, "=")) {
    consume(&tok, tok, "=");

    if (ex_decl->decl_specs->has_typedef)
      error_token(tok, "cannot typedef with initialize");

    if (equal(tok, "{"))
      ex_decl->declarator->init_expr = parse_initialize_list(&tok, tok, state);
    else
      ex_decl->declarator->init_expr = parse_assign(&tok, tok, state);
  }

  Declarator *cur = ex_decl->declarator;
  while (equal(tok, ",")) {
    consume(&tok, tok, ",");
    cur->next = parse_declarator(&tok, tok, state);

    if (equal(tok, "=")) {
      consume(&tok, tok, "=");

      if (ex_decl->decl_specs->has_typedef)
        error_token(tok, "cannot typedef with initialize");

      if (equal(tok, "{"))
        cur->next->init_expr = parse_initialize_list(&tok, tok, state);
      else
        cur->next->init_expr = parse_assign(&tok, tok, state);
    }
    cur = cur->next;
  }

  expect(&tok, tok, ";");

  if (ex_decl->decl_specs->has_typedef) {
    char *def_name = getname_declarator(ex_decl->declarator);
    Typedef *new_def = calloc(1, sizeof(Typedef));
    new_def->name = def_name;

    add_str_dict(state->glb_typedef_dict, new_def->name, new_def);
  }

  *rest = tok;
  return ex_decl;
}

Tree *parse_initialize_list(Token **rest, Token *tok, Analyze *state) {
  expect(&tok, tok, "{");

  InitializeList *head = calloc(1, sizeof(InitializeList));
  InitializeList *cur = head;

  while (!(equal(tok, "}") || (equal(tok, ",") && equal(tok->next, "}")))) {
    InitializeList *now = calloc(1, sizeof(InitializeList));

    if (equal(tok, "[")) {
      not_implemented_token(tok);
    } else if (equal(tok, ".")) {
      not_implemented_token(tok);
    }

    if (equal(tok, "{")) {
      not_implemented_token(tok);
    } else {
      now->init_val = parse_assign(&tok, tok, state);
    }

    cur->next = now;
    cur = now;

    consume(&tok, tok, ",");
  }
  consume(&tok, tok, "}");
  *rest = tok;

  Tree *init_list = calloc(1, sizeof(Tree));
  init_list->kind = INITIALIZE_LIST;
  init_list->init_list = head->next;
  return init_list;
}

bool is_decl_specs(Token *tok, Analyze *state) {
  return equal_kind(tok, TK_LONG) || equal_kind(tok, TK_INT) ||
         equal_kind(tok, TK_SHORT) || equal_kind(tok, TK_CHAR) ||
         equal_kind(tok, TK_VOID) || equal_kind(tok, TK_BOOL) ||
         equal_kind(tok, TK_SIGNED) || equal_kind(tok, TK_UNSIGNED) ||
         equal_kind(tok, TK_FLOAT) || equal_kind(tok, TK_DOUBLE) ||
         equal_kind(tok, TK_STRUCT) || equal_kind(tok, TK_UNION) ||
         equal_kind(tok, TK_ENUM) || equal_kind(tok, TK_CONST) ||
         equal_kind(tok, TK_EXTERN) || equal_kind(tok, TK_STATIC) ||
         equal_kind(tok, TK_INLINE) || equal_kind(tok, TK_NORETURN) ||
         equal_kind(tok, TK_TYPEDEF) ||
         (equal_kind(tok, TK_IDENT) && find_typedef(state, tok->ident_str));
}

bool is_primitive_type_token(Token *tok) {
  return equal_kind(tok, TK_LONG) || equal_kind(tok, TK_INT) ||
         equal_kind(tok, TK_SHORT) || equal_kind(tok, TK_CHAR) ||
         equal_kind(tok, TK_VOID) || equal_kind(tok, TK_BOOL) ||
         equal_kind(tok, TK_SIGNED) || equal_kind(tok, TK_UNSIGNED) ||
         equal_kind(tok, TK_FLOAT) || equal_kind(tok, TK_DOUBLE);
}

DeclSpec *parse_decl_specs(Token **rest, Token *tok, Analyze *state) {
  DeclSpec *decl_spec = calloc(1, sizeof(DeclSpec));
  bool is_complete_type_parse = false;

  PrimitiveTypeToken *primitive_type_token =
      calloc(1, sizeof(PrimitiveTypeToken));
  bool is_primitive_type = false;

  while (is_decl_specs(tok, state)) {
    if (equal_kind(tok, TK_CONST)) {
      consume_kind(&tok, tok, TK_CONST);
      decl_spec->has_const = true;
    } else if (equal_kind(tok, TK_EXTERN)) {
      consume_kind(&tok, tok, TK_EXTERN);
      decl_spec->has_extern = true;
    } else if (equal_kind(tok, TK_STATIC)) {
      consume_kind(&tok, tok, TK_STATIC);
      decl_spec->has_static = true;
    } else if (equal_kind(tok, TK_INLINE)) {
      consume_kind(&tok, tok, TK_INLINE);
      decl_spec->has_inline = true;
    } else if (equal_kind(tok, TK_NORETURN)) {
      consume_kind(&tok, tok, TK_NORETURN);
      decl_spec->has_noreturn = true;
    } else if (equal_kind(tok, TK_TYPEDEF)) {
      consume_kind(&tok, tok, TK_TYPEDEF);
      decl_spec->has_typedef = true;
    } else if (is_primitive_type_token(tok)) {
      if (is_complete_type_parse)
        error("dup type");
      parse_primitive_type_spec(&tok, tok, primitive_type_token);
      is_primitive_type = true;
    } else if (equal_kind(tok, TK_IDENT) &&
               find_typedef(state, tok->ident_str)) {
      if (is_complete_type_parse)
        error("dup type");
      if (is_primitive_type)
        error("dup type");
      decl_spec->def_name = getname_ident(&tok, tok);
      decl_spec->type_spec_kind = TypeSpec_TYPEDEF_NAME;
      is_complete_type_parse = true;
    } else if (equal_kind(tok, TK_STRUCT)) {
      if (is_complete_type_parse)
        error("dup type");
      if (is_primitive_type)
        error("dup type");
      decl_spec->st_spec = parse_struct_spec(&tok, tok, state);
      decl_spec->type_spec_kind = TypeSpec_STRUCT;

      is_complete_type_parse = true;
    } else if (equal_kind(tok, TK_UNION)) {
      if (is_complete_type_parse)
        error("dup type");
      if (is_primitive_type)
        error("dup type");
      decl_spec->union_spec = parse_union_spec(&tok, tok, state);
      decl_spec->type_spec_kind = TypeSpec_UNION;

      is_complete_type_parse = true;
    } else if (equal_kind(tok, TK_ENUM)) {
      if (is_complete_type_parse)
        error("dup type");
      if (is_primitive_type)
        error("dup type");
      decl_spec->en_spec = parse_enum_spec(&tok, tok, state);
      decl_spec->type_spec_kind = TypeSpec_ENUM;

      is_complete_type_parse = true;
    } else {
      not_implemented_token(tok);
    }
  }

  if (is_primitive_type)
    set_primitive_type_spec_kind(decl_spec, primitive_type_token);

  *rest = tok;
  return decl_spec;
}

void parse_primitive_type_spec(Token **rest, Token *tok,
                               PrimitiveTypeToken *primitive_type_token) {
  if (equal_kind(tok, TK_VOID)) {
    consume_kind(&tok, tok, TK_VOID);
    primitive_type_token->void_count++;
  } else if (equal_kind(tok, TK_BOOL)) {
    consume_kind(&tok, tok, TK_BOOL);
    primitive_type_token->bool_count++;
  } else if (equal_kind(tok, TK_CHAR)) {
    consume_kind(&tok, tok, TK_CHAR);
    primitive_type_token->char_count++;
  } else if (equal_kind(tok, TK_SHORT)) {
    consume_kind(&tok, tok, TK_SHORT);
    primitive_type_token->short_count++;
  } else if (equal_kind(tok, TK_INT)) {
    consume_kind(&tok, tok, TK_INT);
    primitive_type_token->int_count++;
  } else if (equal_kind(tok, TK_LONG)) {
    consume_kind(&tok, tok, TK_LONG);
    primitive_type_token->long_count++;
  } else if (equal_kind(tok, TK_SIGNED)) {
    consume_kind(&tok, tok, TK_SIGNED);
    primitive_type_token->signed_count++;
  } else if (equal_kind(tok, TK_UNSIGNED)) {
    consume_kind(&tok, tok, TK_UNSIGNED);
    primitive_type_token->unsigned_count++;
  } else if (equal_kind(tok, TK_FLOAT)) {
    consume_kind(&tok, tok, TK_FLOAT);
    primitive_type_token->float_count++;
  } else if (equal_kind(tok, TK_DOUBLE)) {
    consume_kind(&tok, tok, TK_DOUBLE);
    primitive_type_token->double_count++;
  } else {
    not_implemented_token(tok);
  }

  *rest = tok;
}

Tree *parse_struct_declaration(Token **rest, Token *tok, Analyze *state) {
  Tree *st_decl = calloc(1, sizeof(Tree));
  st_decl->kind = DECLARATION;
  st_decl->decl_specs = parse_decl_specs(&tok, tok, state);

  if (equal(tok, ";"))
    not_implemented_token(tok);

  st_decl->declarator = parse_declarator(&tok, tok, state);
  if (equal(tok, ":"))
    not_implemented_token(tok);

  Declarator *cur = st_decl->declarator;
  while (equal(tok, ",")) {
    consume(&tok, tok, ",");

    cur->next = parse_declarator(&tok, tok, state);
    if (equal(tok, ":"))
      not_implemented_token(tok);

    cur = cur->next;
  }

  expect(&tok, tok, ";");
  *rest = tok;
  return st_decl;
}

StructSpec *parse_struct_spec(Token **rest, Token *tok, Analyze *state) {
  consume_kind(&tok, tok, TK_STRUCT);
  StructSpec *st_spec = calloc(1, sizeof(StructSpec));
  if (equal_kind(tok, TK_IDENT)) {
    st_spec->st_name = getname_ident(&tok, tok);

    if (consume(&tok, tok, "{")) {
      st_spec->has_decl = true;
      Tree *head = calloc(1, sizeof(Tree));
      Tree *cur = head;
      while (!consume(&tok, tok, "}")) {
        cur->next = parse_struct_declaration(&tok, tok, state);
        cur = cur->next;
        consume(&tok, tok, ",");
      }

      st_spec->members = head->next;
    }
  } else {
    expect(&tok, tok, "{");
    st_spec->has_decl = true;
    Tree *head = calloc(1, sizeof(Tree));
    Tree *cur = head;
    while (!consume(&tok, tok, "}")) {
      cur->next = parse_struct_declaration(&tok, tok, state);
      cur = cur->next;
      consume(&tok, tok, ",");
    }

    st_spec->members = head->next;
  }
  *rest = tok;
  return st_spec;
}

UnionSpec *parse_union_spec(Token **rest, Token *tok, Analyze *state) {
  consume_kind(&tok, tok, TK_UNION);
  UnionSpec *union_spec = calloc(1, sizeof(UnionSpec));
  if (equal_kind(tok, TK_IDENT)) {
    union_spec->union_name = getname_ident(&tok, tok);

    if (consume(&tok, tok, "{")) {
      union_spec->has_decl = true;
      Tree *head = calloc(1, sizeof(Tree));
      Tree *cur = head;
      while (!consume(&tok, tok, "}")) {
        cur->next = parse_struct_declaration(&tok, tok, state);
        cur = cur->next;
        consume(&tok, tok, ",");
      }

      union_spec->members = head->next;
    }
  } else {
    expect(&tok, tok, "{");
    union_spec->has_decl = true;
    Tree *head = calloc(1, sizeof(Tree));
    Tree *cur = head;
    while (!consume(&tok, tok, "}")) {
      cur->next = parse_struct_declaration(&tok, tok, state);
      cur = cur->next;
      consume(&tok, tok, ",");
    }

    union_spec->members = head->next;
  }
  *rest = tok;
  return union_spec;
}

EnumSpec *parse_enum_spec(Token **rest, Token *tok, Analyze *state) {
  consume_kind(&tok, tok, TK_ENUM);

  EnumSpec *en_spec = calloc(1, sizeof(EnumSpec));

  if (equal_kind(tok, TK_IDENT)) {

    en_spec->en_name = getname_ident(&tok, tok);

    if (equal(tok, "{")) {
      consume(&tok, tok, "{");
      en_spec->has_decl = true;

      EnumVal *head = calloc(1, sizeof(EnumVal));
      EnumVal *cur = head;
      while (!consume(&tok, tok, "}")) {
        EnumVal *en_val = calloc(1, sizeof(EnumVal));

        en_val->name = getname_ident(&tok, tok);
        if (equal(tok, "=")) {
          consume(&tok, tok, "=");
          en_val->val_expr = parse_constant_expr(&tok, tok, state);
        }

        cur->next = en_val;
        cur = cur->next;
        consume(&tok, tok, ",");
      }

      en_spec->members = head->next;
    }
  } else {
    consume(&tok, tok, "{");
    en_spec->has_decl = true;

    EnumVal *head = calloc(1, sizeof(EnumVal));
    EnumVal *cur = head;
    while (!consume(&tok, tok, "}")) {
      EnumVal *en_val = calloc(1, sizeof(EnumVal));

      en_val->name = getname_ident(&tok, tok);
      if (equal(tok, "=")) {
        consume(&tok, tok, "=");
        en_val->val_expr = parse_constant_expr(&tok, tok, state);
      }

      cur->next = en_val;
      cur = cur->next;
      consume(&tok, tok, ",");
    }

    en_spec->members = head->next;
  }

  *rest = tok;
  return en_spec;
}

bool check_primitive_type_token(PrimitiveTypeToken *primitive_type_token,
                                int void_count, int bool_count, int char_count,
                                int short_count, int int_count, int long_count,
                                int signed_count, int unsigned_count,
                                int float_count, int double_count) {
  return primitive_type_token->void_count == void_count &&
         primitive_type_token->bool_count == bool_count &&
         primitive_type_token->char_count == char_count &&
         primitive_type_token->short_count == short_count &&
         primitive_type_token->int_count == int_count &&
         primitive_type_token->long_count == long_count &&
         primitive_type_token->signed_count == signed_count &&
         primitive_type_token->unsigned_count == unsigned_count &&
         primitive_type_token->float_count == float_count &&
         primitive_type_token->double_count == double_count;
}

void set_primitive_type_spec_kind(DeclSpec *decl_spec,
                                  PrimitiveTypeToken *primitive_type_token) {
  if (check_primitive_type_token(primitive_type_token, 1, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0)) {
    // void
    decl_spec->type_spec_kind = TypeSpec_VOID;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0)) {
    // char
    decl_spec->type_spec_kind = TypeSpec_CHAR;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 1, 0, 0, 0,
                                        1, 0, 0, 0)) {
    // signed char
    decl_spec->type_spec_kind = TypeSpec_CHAR;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 1, 0, 0, 0,
                                        0, 1, 0, 0)) {
    // unsigned char
    // TODO impl unsigned type
    decl_spec->type_spec_kind = TypeSpec_CHAR;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 1, 0, 0,
                                        0, 0, 0, 0)) {
    // short
    decl_spec->type_spec_kind = TypeSpec_INT;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 1, 0, 0,
                                        0, 1, 0, 0)) {
    // unsigned short
    // TODO impl unsigned type
    decl_spec->type_spec_kind = TypeSpec_INT;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 1, 0,
                                        0, 0, 0, 0)) {
    // int
    decl_spec->type_spec_kind = TypeSpec_INT;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 0,
                                        0, 1, 0, 0) ||
             check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 1, 0,
                                        0, 1, 0, 0)) {
    // unsigned
    // TODO impl unsigned type
    decl_spec->type_spec_kind = TypeSpec_INT;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 1,
                                        0, 0, 0, 0) ||
             check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 1, 1,
                                        0, 0, 0, 0)) {
    // long
    decl_spec->type_spec_kind = TypeSpec_LONG;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 1,
                                        0, 1, 0, 0)) {
    // unsigned long
    // TODO impl unsigned type
    decl_spec->type_spec_kind = TypeSpec_LONG;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 2,
                                        0, 0, 0, 0)) {
    // long long
    decl_spec->type_spec_kind = TypeSpec_LONG;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 2,
                                        0, 1, 0, 0)) {
    // unsigned long long
    // TODO impl unsigned type
    decl_spec->type_spec_kind = TypeSpec_LONG;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 0,
                                        0, 0, 1, 0)) {
    // float
    // TODO impl float type
    decl_spec->type_spec_kind = TypeSpec_INT;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 1)) {
    // double
    // TODO impl double type
    decl_spec->type_spec_kind = TypeSpec_LONG;
  } else if (check_primitive_type_token(primitive_type_token, 0, 0, 0, 0, 0, 1,
                                        0, 0, 0, 1)) {
    // long double
    // TODO impl long double type
    decl_spec->type_spec_kind = TypeSpec_LONG;
  } else if (check_primitive_type_token(primitive_type_token, 0, 1, 0, 0, 0, 0,
                                        0, 0, 0, 0)) {
    // _Bool
    decl_spec->type_spec_kind = TypeSpec_BOOL;
  } else {
    /*
    error("void: %d\nbool: %d\nchar: %d\nshort: %d\nint %d\nlong: %d\nsigned: "
          "%d\nunsigned: %d\n float: %d\ndouble: %d\n",
          primitive_type_token->void_count, primitive_type_token->bool_count,
          primitive_type_token->char_count, primitive_type_token->short_count,
          primitive_type_token->int_count, primitive_type_token->long_count,
          primitive_type_token->signed_count,
          primitive_type_token->unsigned_count,
          primitive_type_token->float_count,
          primitive_type_token->double_count);
          */
    not_implemented(__func__);
  }
}

Declarator *parse_declarator(Token **rest, Token *tok, Analyze *state) {
  Declarator *declarator = calloc(1, sizeof(Declarator));

  Pointer **cur = &declarator->pointer;
  while (equal(tok, "*")) {
    consume(&tok, tok, "*");

    // ignore const,restrict
    if (equal_kind(tok, TK_CONST)) {
      consume_kind(&tok, tok, TK_CONST);
    } else if (equal_kind(tok, TK_RESTRICT)) {
      consume_kind(&tok, tok, TK_RESTRICT);
    }

    *cur = calloc(1, sizeof(Pointer));
    cur = &(*cur)->nest;
  }

  // parse ident or nest-declarator
  if (equal_kind(tok, TK_IDENT)) {
    declarator->name = getname_ident(&tok, tok);
  } else if (equal(tok, "(")) {
    consume(&tok, tok, "(");
    declarator->nest = parse_declarator(&tok, tok, state);
    expect(&tok, tok, ")");
  } else {
    error("cannot parse declarator");
  }

  // parse type-suffix
  declarator->type_suffix_kind = NONE;
  if (equal(tok, "(")) {
    expect(&tok, tok, "(");
    declarator->type_suffix_kind = FUNC_DECLARATOR;
    if (!equal(tok, ")") &&
        !(equal_kind(tok, TK_VOID) && equal(tok->next, ")")))
      declarator->args = parse_parameter_type_list(&tok, tok, state);

    for (Tree *cur = declarator->args; cur; cur = cur->next)
      if (cur->has_variable_arg)
        declarator->has_variable_arg = true;

    consume_kind(&tok, tok, TK_VOID);
    consume(&tok, tok, ")");
  } else if (equal(tok, "[")) {
    declarator->type_suffix_kind = ARRAY_DECLARATOR;
    while (consume(&tok, tok, "[")) {
      ArrayDeclarator *arr_decl = calloc(1, sizeof(ArrayDeclarator));
      if (equal(tok, "]")) {
        arr_decl->is_null_size = true;
      } else {
        arr_decl->size = parse_expr(&tok, tok, state);
      }
      consume(&tok, tok, "]");

      arr_decl->next = declarator->arr_decl;
      declarator->arr_decl = arr_decl;
    }
  }

  *rest = tok;
  return declarator;
}

Tree *parse_parameter_type_list(Token **rest, Token *tok, Analyze *state) {
  Tree *head;
  if (is_declaration(tok, state)) {
    head = calloc(1, sizeof(Tree));
    head->kind = DECLARATION;
    head->decl_specs = parse_decl_specs(&tok, tok, state);
    head->declarator = parse_declarator(&tok, tok, state);
    head->nth_arg = 1;
  } else {
    head = parse_type_name(&tok, tok, state);
  }

  Tree *cur = head;
  int count = 2;

  while (consume(&tok, tok, ",")) {
    Tree *node;
    if (equal(tok, "...")) {
      consume(&tok, tok, "...");
      cur->has_variable_arg = true;

      if (equal(tok, ","))
        error("expected )");

      *rest = tok;
      return head;
    } else if (is_declaration(tok, state)) {
      node = calloc(1, sizeof(Tree));
      node->kind = DECLARATION;
      node->decl_specs = parse_decl_specs(&tok, tok, state);
      node->declarator = parse_declarator(&tok, tok, state);
      node->nth_arg = count;
    } else {
      node = parse_type_name(&tok, tok, state);
    }

    count++;
    cur->next = node;
    cur = node;
  }

  *rest = tok;
  return head;
}

Tree *parse_type_name(Token **rest, Token *tok, Analyze *state) {
  DeclSpec *decl_spec = parse_decl_specs(&tok, tok, state);
  Declarator *declarator = parse_abstract_declarator(&tok, tok, state);

  Tree *ret = calloc(1, sizeof(Tree));
  ret->kind = TYPE_NAME;
  ret->decl_specs = decl_spec;
  ret->declarator = declarator;

  *rest = tok;
  return ret;
}

Declarator *parse_abstract_declarator(Token **rest, Token *tok,
                                      Analyze *state) {
  Declarator *declarator = calloc(1, sizeof(Declarator));

  Pointer **cur = &declarator->pointer;
  while (equal(tok, "*")) {
    consume(&tok, tok, "*");

    // ignore const
    if (equal_kind(tok, TK_CONST)) {
      consume_kind(&tok, tok, TK_CONST);
    } else if (equal_kind(tok, TK_RESTRICT)) {
      consume_kind(&tok, tok, TK_RESTRICT);
    }

    *cur = calloc(1, sizeof(Pointer));
    cur = &(*cur)->nest;
  }

  if (equal(tok, "(") && !is_decl_specs(tok->next, state)) {
    consume(&tok, tok, "(");
    declarator->nest = parse_abstract_declarator(&tok, tok, state);
    expect(&tok, tok, ")");
  }

  // parse type-suffix
  declarator->type_suffix_kind = NONE;
  if (equal(tok, "(")) {
    expect(&tok, tok, "(");
    declarator->type_suffix_kind = FUNC_DECLARATOR;

    if (!equal(tok, ")") &&
        !(equal_kind(tok, TK_VOID) && equal(tok->next, ")")))
      declarator->args = parse_parameter_type_list(&tok, tok, state);

    for (Tree *cur = declarator->args; cur; cur = cur->next)
      if (cur->has_variable_arg)
        declarator->has_variable_arg = true;

    consume_kind(&tok, tok, TK_VOID);
    consume(&tok, tok, ")");
  } else if (equal(tok, "[")) {
    declarator->type_suffix_kind = ARRAY_DECLARATOR;
    while (consume(&tok, tok, "[")) {
      ArrayDeclarator *arr_decl = calloc(1, sizeof(ArrayDeclarator));
      if (equal(tok, "]")) {
        arr_decl->is_null_size = true;
      } else {
        arr_decl->size = parse_expr(&tok, tok, state);
      }
      consume(&tok, tok, "]");

      arr_decl->next = declarator->arr_decl;
      declarator->arr_decl = arr_decl;
    }
  }

  *rest = tok;
  return declarator;
}

bool is_declaration(Token *tok, Analyze *state) {
  if (!is_decl_specs(tok, state))
    return false;
  parse_decl_specs(&tok, tok, state);

  return is_declarator(tok, state);
}

bool is_declarator(Token *tok, Analyze *state) {
  while (equal(tok, "*")) {
    consume(&tok, tok, "*");
    consume_kind(&tok, tok, TK_CONST);
  }

  if (equal_kind(tok, TK_IDENT)) {
    consume_kind(&tok, tok, TK_IDENT);
  } else if (equal(tok, "(")) {
    consume(&tok, tok, "(");
    if (!is_declarator(tok, state))
      return false;
    parse_declarator(&tok, tok, state);
    if (!equal(tok, ")"))
      return false;
    consume(&tok, tok, ")");
  } else {
    return false;
  }

  if (equal(tok, "(")) {
    not_implemented_token(tok);
  } else if (equal(tok, "[")) {
    consume(&tok, tok, "[");
    if (!equal_kind(tok, TK_NUM))
      return false;
    consume_kind(&tok, tok, TK_NUM);
    if (!equal(tok, "]"))
      return false;
    consume(&tok, tok, "]");
  }

  return true;
}

Tree *parse_stmt(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_label_stmt(Token **rest, Token *tok, Analyze *state) {
  Tree *node = NULL;
  if (equal_kind(tok, TK_IDENT) && equal(tok->next, ":")) {
    char *label_str = getname_ident(&tok, tok);
    consume(&tok, tok, ":");

    Tree *lhs = parse_stmt(&tok, tok, state);
    Tree *node = new_binary_node(LABEL, lhs, NULL);
    node->label_name = label_str;

    *rest = tok;
    return node;
  } else if (equal_kind(tok, TK_CASE)) {
    consume_kind(&tok, tok, TK_CASE);
    Tree *expr = parse_constant_expr(&tok, tok, state);
    consume(&tok, tok, ":");
    Tree *lhs = parse_stmt(&tok, tok, state);

    *rest = tok;
    Tree *node = new_binary_node(CASE, lhs, NULL);
    node->case_num_node = expr;

    return node;

  } else if (equal_kind(tok, TK_DEFAULT)) {
    consume_kind(&tok, tok, TK_DEFAULT);
    consume(&tok, tok, ":");
    Tree *lhs = parse_stmt(&tok, tok, state);

    *rest = tok;
    return new_binary_node(DEFAULT, lhs, NULL);
  }
  return node;
}

Tree *parse_compound_stmt(Token **rest, Token *tok, Analyze *state) {
  expect(&tok, tok, "{");
  Tree *head = calloc(1, sizeof(Tree));
  Tree *cur = head;
  while (!consume(&tok, tok, "}")) {
    if (is_decl_specs(tok, state)) {
      cur->next = parse_external_decl(&tok, tok, state, false);
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

Tree *parse_jump_stmt(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_iteration_stmt(Token **rest, Token *tok, Analyze *state) {
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

    // parse init
    if (!equal(tok, ";")) {
      if (is_decl_specs(tok, state))
        node->for_init = parse_external_decl(&tok, tok, state, false);
      else {
        node->for_init = parse_expr(&tok, tok, state);
        consume(&tok, tok, ";");
      }
    } else {
      consume(&tok, tok, ";");
    }

    // parse cond
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

Tree *parse_selection_stmt(Token **rest, Token *tok, Analyze *state) {
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
    consume_kind(&tok, tok, TK_SWITCH);
    expect(&tok, tok, "(");
    Tree *cond = parse_expr(&tok, tok, state);
    expect(&tok, tok, ")");

    Tree *lhs = parse_stmt(&tok, tok, state);
    *rest = tok;

    Tree *node = calloc(1, sizeof(Tree));
    node->kind = SWITCH;
    node->cond = cond;
    node->lhs = lhs;

    return node;
  } else {
    error("cannot parse selection_stmt");
  }

  *rest = tok;
  return node;
}

Tree *parse_expr_stmt(Token **rest, Token *tok, Analyze *state) {
  if (equal(tok, ";")) {
    not_implemented_token(tok);
  }

  Tree *node = parse_expr(&tok, tok, state);
  expect(rest, tok, ";");
  return node;
}

Tree *parse_expr(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_assign(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_constant_expr(Token **rest, Token *tok, Analyze *state) {
  return parse_conditional(rest, tok, state);
}

Tree *parse_conditional(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_logical_or(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_logical_and(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_bit_or(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_bit_xor(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_bit_and(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_equality(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_relational(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_shift(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_add(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_mul(Token **rest, Token *tok, Analyze *state) {
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

Tree *parse_cast(Token **rest, Token *tok, Analyze *state) {
  if (equal(tok, "(") && is_decl_specs(tok->next, state)) {

    consume(&tok, tok, "(");
    Tree *node = calloc(1, sizeof(Tree));
    node->kind = CAST;
    node->type_name = parse_type_name(&tok, tok, state);
    expect(&tok, tok, ")");
    node->lhs = parse_cast(&tok, tok, state);

    *rest = tok;
    return node;
  }

  Tree *node = parse_unary(rest, tok, state);
  return node;
}

Tree *parse_unary(Token **rest, Token *tok, Analyze *state) {
  if (equal_kind(tok, TK_SIZEOF)) {
    consume_kind(&tok, tok, TK_SIZEOF);

    if (equal(tok, "(") && is_decl_specs(tok->next, state)) {
      expect(&tok, tok, "(");
      Tree *typename = parse_type_name(&tok, tok, state);
      expect(&tok, tok, ")");
      *rest = tok;
      return new_binary_node(SIZEOF, typename, NULL);
    } else {
      Tree *unary = parse_unary(&tok, tok, state);
      *rest = tok;
      return new_binary_node(SIZEOF, unary, NULL);
    }
  }

  if (equal_kind(tok, TK_ALIGNOF)) {
    consume_kind(&tok, tok, TK_ALIGNOF);
    expect(&tok, tok, "(");
    Tree *typename = parse_type_name(&tok, tok, state);
    expect(&tok, tok, ")");
    *rest = tok;
    return new_binary_node(ALIGNOF, typename, NULL);
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

Tree *parse_postfix(Token **rest, Token *tok, Analyze *state) {
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
      consume(&tok, tok, ".");

      Tree *node = calloc(1, sizeof(Tree));
      node->kind = DOT;
      node->lhs = lhs;
      node->member_name = getname_ident(&tok, tok);

      lhs = node;
    } else if (equal(tok, "->")) {
      consume(&tok, tok, "->");

      Tree *node = calloc(1, sizeof(Tree));
      node->kind = ARROW;
      node->lhs = lhs;
      node->member_name = getname_ident(&tok, tok);

      lhs = node;
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

bool is_builtin(Token *tok) {
  return cmp_ident(tok, "__builtin_va_start") ||
         cmp_ident(tok, "__builtin_va_end");
}

Tree *parse_primary(Token **rest, Token *tok, Analyze *state) {
  Tree *primary;

  if (equal_kind(tok, TK_NUM)) {
    primary = calloc(1, sizeof(Tree));
    Token *num_tok = consume_kind(&tok, tok, TK_NUM);
    primary->kind = NUM;
    primary->num = num_tok->val;
    primary->is_long = num_tok->is_long;
  } else if (equal_kind(tok, TK_STR)) {
    primary = calloc(1, sizeof(Tree));
    StrLiteral *str_literal = consume_kind(&tok, tok, TK_STR)->str_literal;

    primary->kind = STR;
    primary->str_literal = str_literal;
  } else if (is_builtin(tok)) {
    primary = parse_builtin(&tok, tok, state);
  } else if (equal_kind(tok, TK_IDENT)) {
    primary = calloc(1, sizeof(Tree));
    primary->kind = VAR;
    primary->var_name = getname_ident(&tok, tok);
  } else if (equal(tok, "(")) {
    consume(&tok, tok, "(");
    primary = parse_expr(&tok, tok, state);
    expect(&tok, tok, ")");
  } else {
    error_token(tok, "cannot parse primary");
  }

  *rest = tok;
  return primary;
}

Tree *parse_builtin(Token **rest, Token *tok, Analyze *state) {
  Tree *node = calloc(1, sizeof(Tree));
  if (cmp_ident(tok, "__builtin_va_start")) {
    expect_ident(&tok, tok, "__builtin_va_start");
    node->kind = BUILTIN_VA_START;

    expect(&tok, tok, "(");
    node->lhs = parse_assign(&tok, tok, state);
    expect(&tok, tok, ",");
    node->rhs = parse_assign(&tok, tok, state);
    expect(&tok, tok, ")");
  } else if (cmp_ident(tok, "__builtin_va_end")) {
    expect_ident(&tok, tok, "__builtin_va_end");
    node->kind = BUILTIN_VA_END;

    expect(&tok, tok, "(");
    node->lhs = parse_assign(&tok, tok, state);
    expect(&tok, tok, ")");

  } else
    not_implemented_token(tok);

  *rest = tok;
  return node;
}
