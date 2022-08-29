
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "error.h"
#include "parse.h"
#include "type.h"

#ifndef __STDC__

#include "selfhost_util.h"

void *calloc();
size_t strlen();
int memcmp();
void *memcpy();

#endif

static void analyze_external_decl(Tree *ast, Analyze *state);
static void analyze_decl_spec(DeclSpec *decl_spec, Analyze *state,
                              bool is_global);
static void analyze_parameter(Tree *arg, Analyze *state);
static void analyze_stmt(Tree *ast, Analyze *state);

static void push_lvar_scope(ObjScope **lscope);
static void pop_lvar_scope(ObjScope **lscope);
static void push_lvar(ObjScope *locals, Obj *lvar);
static Obj *find_lvar(ObjScope *locals, char *lvar_name, int lvar_len);
static Obj *find_global(Obj *globals, char *var_name, int var_len);

static void push_label(LabelScope **lscope, int label_number);
static void pop_label(LabelScope **lscope);

static void push_switch(SwitchScope **switch_scope, Tree *switch_node);
static void pop_switch(SwitchScope **switch_scope);

static StructDef *find_struct(Analyze *state, char *struct_name);
static UnionDef *find_union(Analyze *state, char *union_name);
static Member *find_struct_member(StructDef *st_def, char *mem_name,
                                  int mem_len);
static Member *find_union_member(UnionDef *union_def, char *mem_name,
                                 int mem_len);
static EnumDef *find_enum(EnumDef *en_defs, char *en_name, int en_len);

static EnumVal *find_enum_val(EnumDef *en_defs, char *name, int len);

Analyze *new_analyze_state() {
  Analyze *state = calloc(1, sizeof(Analyze));
  state->glb_struct_def_dict = new_str_dict();
  state->glb_union_def_dict = new_str_dict();
  state->glb_typedef_dict = new_str_dict();
  state->label_cnt = 0;
  return state;
}

void analyze_translation_unit(Tree *ast) {
  Analyze *state = new_analyze_state();

  builtin_type_init(state);

  Tree *cur = ast;
  while (cur) {
    analyze_external_decl(cur, state);
    cur = cur->next;
  }
}

void analyze_external_decl(Tree *ast, Analyze *state) {
  if (ast->kind == FUNC_DEF) {

    analyze_decl_spec(ast->decl_specs, state, true);

    Type *obj_type = gettype_decl_spec(ast->decl_specs, state);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);
    Tree *args = getargs_declarator(ast->declarator);

    Obj *func = calloc(1, sizeof(Obj));
    func->obj_name = obj_name;
    func->obj_len = strlen(obj_name);
    func->type = obj_type;
    func->locals = calloc(1, sizeof(ObjScope));
    func->is_defined = true;
    func->is_global = true;

    func->stack_size = 0x0;

    state->current_func = func;
    func->next = state->glb_objs;
    state->glb_objs = func;

    ast->def_obj = func;

    Tree *cur = args;
    while (cur) {
      analyze_parameter(cur, state);
      cur = cur->next;
    }

    analyze_stmt(ast->func_body, state);

    func->stack_size = calc_rbp_offset(0, func->stack_size, 8);

  } else if (ast->kind == DECLARATION) {

    analyze_decl_spec(ast->decl_specs, state, true);

    if (!ast->declarator) {
      return;
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs, state);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);

    if (ast->decl_specs->has_typedef) {
      Typedef *new_def = calloc(1, sizeof(Typedef));
      new_def->name = obj_name;
      new_def->type = obj_type;

      add_str_dict(state->glb_typedef_dict, new_def->name, new_def);
    } else {

      if (ast->declarator->init_expr) {
        analyze_stmt(ast->declarator->init_expr, state);
        if (!is_constexpr(ast->declarator->init_expr))
          error("not constexpr");
        if (!is_compatible(obj_type, ast->declarator->init_expr))
          error("not compatible type");
      }

      Obj *obj = calloc(1, sizeof(Obj));
      obj->obj_name = obj_name;
      obj->obj_len = strlen(obj_name);
      obj->type = obj_type;
      obj->is_global = true;

      if (obj_type->kind != FUNC && !ast->decl_specs->has_extern)
        obj->is_defined = true;

      obj->next = state->glb_objs;
      state->glb_objs = obj;

      ast->def_obj = obj;
    }

  } else {
    error("not external_decl");
  }
}

void analyze_decl_spec(DeclSpec *decl_spec, Analyze *state, bool is_global) {
  if (decl_spec->type_spec_kind == TypeSpec_STRUCT) {

    if (!decl_spec->st_spec->st_name) {
      not_implemented(__func__);
    }

    StructDef *st_defs = find_struct(state, decl_spec->st_spec->st_name);

    if (!st_defs) {
      st_defs = calloc(1, sizeof(StructDef));
      st_defs->st_name = decl_spec->st_spec->st_name;

      add_str_dict(state->glb_struct_def_dict, st_defs->st_name, st_defs);
    }

    if (st_defs->is_defined && decl_spec->st_spec->has_decl)
      error("redifine struct");

    if (decl_spec->st_spec->has_decl) {
      st_defs->is_defined = true;
      st_defs->size = 0;
      st_defs->alignment = 1;

      Tree *cur = decl_spec->st_spec->members;
      Member *head = calloc(1, sizeof(Member));
      Member *mem_cur = head;
      while (cur) {
        Member *mem = calloc(1, sizeof(Member));

        analyze_decl_spec(cur->decl_specs, state, false);
        Type *obj_type = gettype_decl_spec(cur->decl_specs, state);
        obj_type = gettype_declarator(cur->declarator, obj_type);

        char *obj_name = getname_declarator(cur->declarator);

        mem->member_name = obj_name;
        mem->member_len = strlen(obj_name);
        mem->type = obj_type;
        mem->offset = (st_defs->size % type_alignment(obj_type) == 0)
                          ? st_defs->size
                          : st_defs->size + type_alignment(obj_type) -
                                st_defs->size % type_alignment(obj_type);

        st_defs->size = mem->offset + type_size(obj_type);
        if (type_alignment(obj_type) > st_defs->alignment)
          st_defs->alignment = type_alignment(obj_type);

        mem_cur->next = mem;
        mem_cur = mem;

        cur = cur->next;
      }

      st_defs->members = head->next;

      st_defs->size = (st_defs->size % st_defs->alignment == 0)
                          ? st_defs->size
                          : st_defs->size + st_defs->alignment -
                                st_defs->size % st_defs->alignment;
    }

    decl_spec->st_def = st_defs;
  } else if (decl_spec->type_spec_kind == TypeSpec_UNION) {

    if (!decl_spec->union_spec->union_name) {
      not_implemented(__func__);
    }

    UnionDef *union_def = find_union(state, decl_spec->union_spec->union_name);

    if (!union_def) {
      union_def = calloc(1, sizeof(UnionDef));
      union_def->union_name = decl_spec->union_spec->union_name;

      add_str_dict(state->glb_union_def_dict, union_def->union_name, union_def);
    }

    if (union_def->is_defined && decl_spec->union_spec->has_decl)
      error("redefine union");

    if (decl_spec->union_spec->has_decl) {
      union_def->is_defined = true;
      union_def->size = 0;
      union_def->alignment = 1;

      Tree *cur = decl_spec->union_spec->members;
      Member *head = calloc(1, sizeof(Member));
      Member *mem_cur = head;

      while (cur) {
        Member *mem = calloc(1, sizeof(Member));

        analyze_decl_spec(cur->decl_specs, state, false);
        Type *obj_type = gettype_decl_spec(cur->decl_specs, state);
        obj_type = gettype_declarator(cur->declarator, obj_type);

        char *obj_name = getname_declarator(cur->declarator);

        mem->member_name = obj_name;
        mem->member_len = strlen(obj_name);
        mem->type = obj_type;
        mem->offset = 0;

        union_def->size =
            (type_size(obj_type) > union_def->size ? type_size(obj_type)
                                                   : union_def->size);
        union_def->alignment = (type_alignment(obj_type) > union_def->alignment
                                    ? type_alignment(obj_type)
                                    : union_def->alignment);
        mem_cur->next = mem;
        mem_cur = mem;

        cur = cur->next;
      }

      union_def->members = head->next;
      union_def->size = (union_def->size % union_def->alignment == 0)
                            ? union_def->size
                            : union_def->size + union_def->alignment -
                                  union_def->size % union_def->alignment;
    }

    decl_spec->union_def = union_def;

  } else if (decl_spec->type_spec_kind == TypeSpec_ENUM) {
    if (!decl_spec->en_spec->en_name)
      not_implemented(__func__);

    EnumDef *en_def = find_enum(state->glb_endefs, decl_spec->en_spec->en_name,
                                decl_spec->en_spec->en_len);

    if (!en_def) {
      en_def = calloc(1, sizeof(EnumDef));
      en_def->en_name = decl_spec->en_spec->en_name;
      en_def->en_len = decl_spec->en_spec->en_len;

      en_def->next = state->glb_endefs;
      state->glb_endefs = en_def;
    }

    if (en_def->is_defined && decl_spec->en_spec->has_decl)
      error("redifine enum");

    if (decl_spec->en_spec->has_decl) {
      en_def->is_defined = true;

      en_def->members = decl_spec->en_spec->members;
      int val = 0;
      for (EnumVal *cur = en_def->members; cur; cur = cur->next) {
        cur->val = val;
        val++;
      }
    }

    decl_spec->en_def = en_def;
  }
}

void analyze_parameter(Tree *ast, Analyze *state) {
  if (ast->kind == DECLARATION) {

    analyze_decl_spec(ast->decl_specs, state, false);

    if (!ast->declarator) {
      return;
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs, state);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    if (obj_type->kind == ARRAY)
      obj_type->kind = PTR;

    char *obj_name = getname_declarator(ast->declarator);

    if (ast->decl_specs->has_typedef) {
      not_implemented(__func__);
    } else {

      Obj *lvar = calloc(1, sizeof(Obj));
      lvar->obj_name = obj_name;
      lvar->obj_len = strlen(obj_name);
      lvar->type = obj_type;
      lvar->nth_arg = ast->nth_arg;
      lvar->rbp_offset =
          calc_rbp_offset(state->current_func->stack_size, type_size(obj_type),
                          type_alignment(obj_type));
      state->current_func->stack_size = lvar->rbp_offset;

      ast->def_obj = lvar;

      push_lvar(state->current_func->locals, lvar);
    }

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

    analyze_decl_spec(ast->decl_specs, state, false);

    if (!ast->declarator) {
      return;
    }

    Type *obj_type = gettype_decl_spec(ast->decl_specs, state);
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

    if (ast->declarator->init_expr) {
      analyze_stmt(ast->declarator->init_expr, state);
    }
  } else if (ast->kind == LABEL) {
    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == CASE) {
    analyze_stmt(ast->case_num_node, state);
    int case_num = eval_constexpr(ast->case_num_node);

    ast->case_num = case_num;

    if (!state->switch_stmts)
      error("not in switch-stmt");

    Case *new_case = calloc(1, sizeof(Case));
    new_case->case_num = case_num;
    new_case->next = state->switch_stmts->switch_node->cases;
    state->switch_stmts->switch_node->cases = new_case;

    ast->label_number = state->switch_stmts->switch_node->label_number;

    analyze_stmt(ast->lhs, state);

  } else if (ast->kind == DEFAULT) {
    if (!state->switch_stmts)
      error("not in switch-stmt");

    if (state->switch_stmts->switch_node->has_default)
      error("already exist default");

    state->switch_stmts->switch_node->has_default = true;
    ast->label_number = state->switch_stmts->switch_node->label_number;

    analyze_stmt(ast->lhs, state);
  } else if (ast->kind == RETURN) {
    if (ast->lhs) {
      analyze_stmt(ast->lhs, state);
      if (!is_compatible(state->current_func->type->return_type, ast->lhs))
        error("invalid return type");
    } else {
      if (state->current_func->type->return_type->kind != VOID)
        error("must return value");
    }
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

    push_lvar_scope(&state->current_func->locals);

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

    pop_lvar_scope(&state->current_func->locals);

  } else if (ast->kind == IF) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);
    analyze_stmt(ast->lhs, state);
    if (ast->rhs)
      analyze_stmt(ast->rhs, state);
  } else if (ast->kind == SWITCH) {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);

    push_label(&state->break_labels, ast->label_number);
    push_switch(&state->switch_stmts, ast);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_switch(&state->switch_stmts);

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
  } else if (ast->kind == CAST) {
    analyze_decl_spec(ast->type_name->decl_specs, state, false);
    Type *cast_type = gettype_decl_spec(ast->type_name->decl_specs, state);
    cast_type = gettype_declarator(ast->type_name->declarator, cast_type);

    analyze_stmt(ast->lhs, state);

    if (!is_integer(cast_type) || !is_integer(ast->lhs->type))
      not_implemented("not integer cast");

    ast->type = cast_type;

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
    ast->type = type_int;
  } else if (ast->kind == BIT_NOT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == SIZEOF) {
    if (ast->lhs->kind == TYPE_NAME) {
      analyze_decl_spec(ast->lhs->decl_specs, state, false);
      Type *base_type = gettype_decl_spec(ast->lhs->decl_specs, state);
      base_type = gettype_declarator(ast->lhs->declarator, base_type);

      // replace "sizeof" -> num
      ast->kind = NUM;
      ast->num = type_size(base_type);
      ast->type = type_int;
    } else {
      analyze_stmt(ast->lhs, state);

      // replace "sizeof" -> num
      ast->kind = NUM;
      ast->num = type_size(ast->lhs->type);
      ast->type = type_int;
    }
  } else if (ast->kind == ALIGNOF) {
    analyze_decl_spec(ast->lhs->decl_specs, state, false);
    Type *base_type = gettype_decl_spec(ast->lhs->decl_specs, state);
    base_type = gettype_declarator(ast->lhs->declarator, base_type);

    // replace "sizeof" -> num
    ast->kind = NUM;
    ast->num = type_alignment(base_type);
    ast->type = type_int;
  } else if (ast->kind == FUNC_CALL) {
    analyze_stmt(ast->lhs, state);
    Tree *cur = ast->call_args;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }

    if (ast->lhs->type->kind == FUNC) {
      ast->type = ast->lhs->type->return_type;
    } else if (ast->lhs->type->kind == PTR &&
               ast->lhs->type->ptr_to->kind == FUNC) {
      ast->type = ast->lhs->type->ptr_to->return_type;
    } else
      error("cannot call func");

  } else if (ast->kind == POST_INCREMENT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == POST_DECREMENT) {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } else if (ast->kind == DOT) {
    analyze_stmt(ast->lhs, state);
    if (ast->lhs->type->kind != STRUCT && ast->lhs->type->kind != UNION)
      error("lhs is not struct");

    Member *member;
    if (ast->lhs->type->kind == STRUCT)
      member = find_struct_member(ast->lhs->type->st_def, ast->member_name,
                                  ast->member_len);
    else
      member = find_union_member(ast->lhs->type->union_def, ast->member_name,
                                 ast->member_len);

    if (!member)
      error("not find member");

    ast->member = member;
    ast->type = member->type;

  } else if (ast->kind == ARROW) {
    analyze_stmt(ast->lhs, state);
    if (ast->lhs->type->kind != PTR ||
        (ast->lhs->type->ptr_to->kind != STRUCT &&
         ast->lhs->type->ptr_to->kind != UNION))
      error("lhs is not ptr to struct");

    Member *member;
    if (ast->lhs->type->ptr_to->kind == STRUCT)
      member = find_struct_member(ast->lhs->type->ptr_to->st_def,
                                  ast->member_name, ast->member_len);
    else
      member = find_union_member(ast->lhs->type->ptr_to->union_def,
                                 ast->member_name, ast->member_len);

    if (!member)
      error("not find member");

    ast->member = member;
    ast->type = member->type;

  } else if (ast->kind == NUM) {
    if (ast->is_long)
      ast->type = type_long;
    else
      ast->type = type_int;
  } else if (ast->kind == STR) {
    ast->type = newtype_ptr(type_char);
  } else if (ast->kind == VAR) {

    // predefined ident
    if (!memcmp(ast->var_name, "__func__", 8)) {

      // make str-literal for func-name
      StrLiteral *func_name = calloc(1, sizeof(StrLiteral));
      func_name->str = calloc(state->current_func->obj_len + 1, sizeof(char));
      func_name->len = state->current_func->obj_len;
      memcpy(func_name->str, state->current_func->obj_name, func_name->len);

      if (!str_literals) {
        func_name->id = 0;
        str_literals = func_name;
      } else {
        func_name->id = str_literals->id + 1;
        func_name->next = str_literals;
        str_literals = func_name;
      }

      // replace ast type
      ast->kind = STR;
      ast->str_literal = func_name;
      ast->type = newtype_ptr(type_char);
      return;
    }

    EnumVal *en_val =
        find_enum_val(state->glb_endefs, ast->var_name, ast->var_len);
    if (en_val) {
      ast->kind = NUM;
      ast->num = en_val->val;
      ast->type = type_int;
      return;
    }

    Obj *var =
        find_lvar(state->current_func->locals, ast->var_name, ast->var_len);

    if (!var) {
      var = find_global(state->glb_objs, ast->var_name, ast->var_len);
      if (!var)
        error("cannot find var: %.*s", ast->var_len, ast->var_name);
    }

    ast->var_obj = var;
    ast->type = var->type;

  } else if (ast->kind == BUILTIN_VA_START) {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    if (ast->lhs->kind != VAR || ast->rhs->kind != VAR)
      error("invalid usage  __builtin_va_start");
  } else if (ast->kind == BUILTIN_VA_END) {
    analyze_stmt(ast->lhs, state);
    if (ast->lhs->kind != VAR)
      error("invalid usage  __builtin_va_end");
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

void push_switch(SwitchScope **switch_scope, Tree *switch_node) {
  SwitchScope *tmp = calloc(1, sizeof(SwitchScope));
  tmp->switch_node = switch_node;
  tmp->next = *switch_scope;
  *switch_scope = tmp;
}

void pop_switch(SwitchScope **switch_scope) {
  *switch_scope = (*switch_scope)->next;
}

int calc_rbp_offset(int start, int data_size, int alignment) {
  return ((start + data_size + alignment - 1) / alignment) * alignment;
}

StructDef *find_struct(Analyze *state, char *struct_name) {
  return find_str_dict(state->glb_struct_def_dict, struct_name);
}

UnionDef *find_union(Analyze *state, char *union_name) {
  return find_str_dict(state->glb_union_def_dict, union_name);
}

Member *find_struct_member(StructDef *st_def, char *mem_name, int mem_len) {
  for (Member *cur = st_def->members; cur; cur = cur->next)
    if (cur->member_len == mem_len &&
        !memcmp(mem_name, cur->member_name, mem_len))
      return cur;
  return NULL;
}

Member *find_union_member(UnionDef *union_def, char *mem_name, int mem_len) {
  for (Member *cur = union_def->members; cur; cur = cur->next)
    if (cur->member_len == mem_len &&
        !memcmp(mem_name, cur->member_name, mem_len))
      return cur;
  return NULL;
}

EnumDef *find_enum(EnumDef *en_defs, char *en_name, int en_len) {
  for (EnumDef *cur = en_defs; cur; cur = cur->next)
    if (cur->en_len == en_len && !memcmp(en_name, cur->en_name, en_len))
      return cur;
  return NULL;
}

EnumVal *find_enum_val(EnumDef *en_defs, char *name, int len) {
  for (EnumDef *cur_def = en_defs; cur_def; cur_def = cur_def->next)
    for (EnumVal *cur = cur_def->members; cur; cur = cur->next)
      if (cur->len == len && !memcmp(name, cur->name, len))
        return cur;
  return NULL;
}

Typedef *find_typedef(Analyze *state, char *typedef_name) {
  return find_str_dict(state->glb_typedef_dict, typedef_name);
}

bool is_constexpr(Tree *expr) {
  if (expr->kind == NUM)
    return true;
  else
    return false;
}

int eval_constexpr(Tree *expr) {
  if (expr->kind == NUM)
    return expr->num;
  else
    not_implemented(__func__);
  return 0;
}
