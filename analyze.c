
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void analyze_external_decl(Tree *ast, Analyze *state);
static void analyze_stmt(Tree *ast, Analyze *state);

static void push_lvar(ObjScope *locals, Obj *lvar);
static Obj *find_lvar(ObjScope *locals, char *lvar_name, int lvar_len);
static Obj *find_global(Obj *globals, char *var_name, int var_len);

void analyze_translation_unit(Tree *ast) {
  Analyze *state = calloc(1, sizeof(Analyze));
  state->globals = calloc(1, sizeof(Obj));
  state->label_cnt = 0;

  Tree *cur = ast;
  while (cur) {
    analyze_external_decl(cur, state);
    cur = cur->next;
  }
}

void analyze_external_decl(Tree *ast, Analyze *state) {
  if (ast->kind == FUNC_DEF) {

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);
    Tree *args = getargs_declarator(ast->declarator);

    Obj *func = calloc(1, sizeof(Obj));
    func->obj_name = obj_name;
    func->obj_len = strlen(obj_name);
    func->type = obj_type;
    func->stack_size = 0;
    func->locals = calloc(1, sizeof(ObjScope));
    func->is_defined = true;
    func->is_global = true;

    state->current_func = func;
    func->next = state->globals;
    state->globals = func;

    ast->def_obj = func;

    Tree *cur = args;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }

    analyze_stmt(ast->func_body, state);

  } else if (ast->kind == DECLARATION) {
    if (!ast->declarator) {
      not_implemented();
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);

    Obj *obj = calloc(1, sizeof(Obj));
    obj->obj_name = obj_name;
    obj->obj_len = strlen(obj_name);
    obj->type = obj_type;
    obj->is_global = true;

    if (obj_type->kind != FUNC)
      obj->is_defined = true;

    obj->next = state->globals;
    state->globals = obj;

    ast->def_obj = obj;

  } else {
    error("not external_decl");
  }
}

void analyze_stmt(Tree *ast, Analyze *state) {
  if (ast->kind == COMPOUND_STMT) {
    Tree *cur = ast->stmts;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }
  } else if (ast->kind == DECLARATION) {
    if (!ast->declarator) {
      not_implemented();
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);

    Obj *lvar = calloc(1, sizeof(Obj));
    lvar->obj_name = obj_name;
    lvar->obj_len = strlen(obj_name);
    lvar->type = obj_type;
    lvar->rbp_offset = offset_alignment(state->current_func->stack_size, 4, 4);
    state->current_func->stack_size = lvar->rbp_offset;

    ast->def_obj = lvar;

    push_lvar(state->current_func->locals, lvar);
  } else if (ast->kind == RETURN) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == COMMA) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == ADD_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == SUB_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == MUL_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == DIV_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == MOD_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == AND_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == OR_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == XOR_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == LSHIFT_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == RSHIFT_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == CONDITIONAL) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == LOGICAL_OR) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == LOGICAL_AND) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == BIT_OR) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == BIT_XOR) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == BIT_AND) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == NOT_EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == SMALLER) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == SMALLER_EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == GREATER) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == GREATER_EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == LSHIFT) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == RSHIFT) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == ADD) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == SUB) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == MUL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == DIV) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == MOD) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
  } else if (ast->kind == PLUS) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == MINUS) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == LOGICAL_NOT) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == BIT_NOT) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == FUNC_CALL) {
    analyze_stmt(ast->lhs, state);
    Tree *cur = ast->call_args;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }
  } else if (ast->kind == NUM) {
  } else if (ast->kind == VAR) {
    Obj *var =
        find_lvar(state->current_func->locals, ast->var_name, ast->var_len);

    if (!var) {
      var = find_global(state->globals, ast->var_name, ast->var_len);
      if (!var)
        error("cannot find var");
    }

    ast->var_obj = var;
    ast->type = var->type;

  } else {
    not_implemented();
  }
}

void push_lvar(ObjScope *locals, Obj *lvar) {
  lvar->next = locals->obj;
  locals->obj = lvar;
}

Obj *find_lvar(ObjScope *locals, char *lvar_name, int lvar_len) {
  for (ObjScope *cur_scope = locals; cur_scope; cur_scope = cur_scope->next)
    for (Obj *cur = cur_scope->obj; cur; cur = cur->next)
      if (cur->obj_len == lvar_len &&
          !memcmp(lvar_name, cur->obj_name, lvar_len))
        return cur;
  return NULL;
}

Obj *find_global(Obj *globals, char *var_name, int var_len) {
  for (Obj *cur = globals; cur; cur = cur->next)
    if (cur->obj_len == var_len && !memcmp(var_name, cur->obj_name, var_len))
      return cur;
  return NULL;
}

int offset_alignment(int start, int data_size, int alignment) {
  return ((start + data_size + alignment - 1) / alignment) * alignment;
}
