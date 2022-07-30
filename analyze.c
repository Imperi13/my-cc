
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void analyze_external_decl(Tree *ast, Analyze *state);
static void analyze_parameter(Tree *arg, Analyze *state);
static void analyze_stmt(Tree *ast, Analyze *state);

static void push_lvar_scope(ObjScope **lscope);
static void pop_lvar_scope(ObjScope **lscope);
static void push_lvar(ObjScope *locals, Obj *lvar);
static Obj *find_lvar(ObjScope *locals, char *lvar_name, int lvar_len);
static Obj *find_global(Obj *globals, char *var_name, int var_len);

static void push_label(LabelScope **lscope, int label_number);
static void pop_label(LabelScope **lscope);

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
      analyze_parameter(cur, state);
      cur = cur->next;
    }

    analyze_stmt(ast->func_body, state);

  } else if (ast->kind == DECLARATION) {
    if (!ast->declarator) {
      not_implemented(__func__);
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

void analyze_parameter(Tree *ast, Analyze *state) {
  if (ast->kind == DECLARATION) {
    if (!ast->declarator) {
      not_implemented(__func__);
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    if (obj_type->kind == ARRAY)
      obj_type->kind = PTR;

    char *obj_name = getname_declarator(ast->declarator);

    Obj *lvar = calloc(1, sizeof(Obj));
    lvar->obj_name = obj_name;
    lvar->obj_len = strlen(obj_name);
    lvar->type = obj_type;
    lvar->rbp_offset =
        calc_rbp_offset(state->current_func->stack_size, type_size(obj_type),
                        type_alignment(obj_type));
    state->current_func->stack_size = lvar->rbp_offset;

    ast->def_obj = lvar;

    push_lvar(state->current_func->locals, lvar);
  } else
    error("cannot analyze parameter");
}

void analyze_stmt(Tree *ast, Analyze *state) {
  if (ast->kind == COMPOUND_STMT) {
    push_lvar_scope(&state->current_func->locals);

    Tree *cur = ast->stmts;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }

    pop_lvar_scope(&state->current_func->locals);
  } else if (ast->kind == DECLARATION) {
    if (!ast->declarator) {
      not_implemented(__func__);
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);

    Obj *lvar = calloc(1, sizeof(Obj));
    lvar->obj_name = obj_name;
    lvar->obj_len = strlen(obj_name);
    lvar->type = obj_type;
    lvar->rbp_offset =
        calc_rbp_offset(state->current_func->stack_size, type_size(obj_type),
                        type_alignment(obj_type));
    state->current_func->stack_size = lvar->rbp_offset;

    ast->def_obj = lvar;

    push_lvar(state->current_func->locals, lvar);
  } else if (ast->kind == RETURN) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == BREAK) {
    if (!state->break_labels)
      error("invalid break stmt");

    ast->label_number = state->break_labels->label_number;
  } else if (ast->kind == CONTINUE) {
    if (!state->continue_labels)
      error("invalid continue stmt");

    ast->label_number = state->continue_labels->label_number;
  } else if (ast->kind == WHILE) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    analyze_stmt(ast->cond, state);

    push_label(&state->break_labels, ast->label_number);
    push_label(&state->continue_labels, ast->label_number);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_label(&state->continue_labels);

  } else if (ast->kind == DO_WHILE) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    analyze_stmt(ast->cond, state);

    push_label(&state->break_labels, ast->label_number);
    push_label(&state->continue_labels, ast->label_number);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_label(&state->continue_labels);

  } else if (ast->kind == FOR) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    if (ast->for_init)
      analyze_stmt(ast->for_init, state);
    analyze_stmt(ast->cond, state);
    if (ast->for_update)
      analyze_stmt(ast->for_update, state);

    push_label(&state->break_labels, ast->label_number);
    push_label(&state->continue_labels, ast->label_number);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_label(&state->continue_labels);

  } else if (ast->kind == IF) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);
    analyze_stmt(ast->lhs, state);
    if (ast->rhs)
      analyze_stmt(ast->rhs, state);
  } else if (ast->kind == COMMA) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->rhs->type;
  } else if (ast->kind == ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == ADD_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == SUB_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == MUL_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == DIV_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == MOD_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == AND_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == OR_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == XOR_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == LSHIFT_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == RSHIFT_ASSIGN) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == CONDITIONAL) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);

    ast->type = ast->lhs->type;
  } else if (ast->kind == LOGICAL_OR) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == LOGICAL_AND) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == BIT_OR) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == BIT_XOR) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == BIT_AND) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == NOT_EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == SMALLER) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == SMALLER_EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == GREATER) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == GREATER_EQUAL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = type_int;
  } else if (ast->kind == LSHIFT) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == RSHIFT) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == ADD) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);

    Type *ltype = ast->lhs->type;
    if (ltype->kind == ARRAY)
      ltype = newtype_ptr(ltype->ptr_to);
    Type *rtype = ast->rhs->type;
    if (rtype->kind == ARRAY)
      rtype = newtype_ptr(rtype->ptr_to);

    if (is_integer(ltype) && is_integer(rtype))
      ast->type = type_int;
    else if (ltype->kind == PTR && is_integer(rtype))
      ast->type = ltype;
    else if (rtype->kind == PTR && is_integer(ltype))
      ast->type = rtype;
    else
      error("unexpected type pair");

  } else if (ast->kind == SUB) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);

    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type))
      ast->type = type_int;
    else if (ast->lhs->type->kind == PTR && is_integer(ast->rhs->type))
      ast->type = ast->lhs->type;
    else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR)
      ast->type = type_int;
    else
      error("unexpected type pair");

  } else if (ast->kind == MUL) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == DIV) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == MOD) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == PLUS) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == MINUS) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == ADDR) {
    analyze_stmt(ast->lhs, state);
    ast->type = newtype_ptr(ast->lhs->type);
  } else if (ast->kind == DEREF) {
    analyze_stmt(ast->lhs, state);

    Type *ltype = ast->lhs->type;
    if (ltype->kind == ARRAY)
      ltype = newtype_ptr(ltype->ptr_to);

    if (ltype->kind != PTR)
      error("cannot deref");
    ast->type = ltype->ptr_to;

  } else if (ast->kind == LOGICAL_NOT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == BIT_NOT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == FUNC_CALL) {
    analyze_stmt(ast->lhs, state);
    Tree *cur = ast->call_args;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }

    if (ast->lhs->type->kind != FUNC)
      error("cannot call func");
    ast->type = ast->lhs->type->return_type;
  } else if (ast->kind == POST_INCREMENT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == POST_DECREMENT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == NUM) {
    ast->type = type_int;
  } else if (ast->kind == STR) {
    ast->type = newtype_ptr(type_char);
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
    not_implemented(__func__);
  }
}

void push_lvar_scope(ObjScope **lscope) {
  ObjScope *lsc = calloc(1, sizeof(ObjScope));
  lsc->next = *lscope;
  *lscope = lsc;
}

void pop_lvar_scope(ObjScope **lscope) { *lscope = (*lscope)->next; }

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

void push_label(LabelScope **lscope, int label_number) {
  LabelScope *tmp = calloc(1, sizeof(LabelScope));
  tmp->label_number = label_number;
  tmp->next = *lscope;
  *lscope = tmp;
}

void pop_label(LabelScope **lscope) { *lscope = (*lscope)->next; }

int calc_rbp_offset(int start, int data_size, int alignment) {
  return ((start + data_size + alignment - 1) / alignment) * alignment;
}

int eval_constexpr(Tree *expr) {
  if (expr->kind == NUM)
    return expr->num;
  else
    not_implemented(__func__);
  return 0;
}
