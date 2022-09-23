
#include <stdlib.h>
#include <string.h>

#include "analyze.h"
#include "constexpr.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void analyze_external_decl(Tree *ast, Analyze *state);
static void analyze_decl_spec(DeclSpec *decl_spec, Analyze *state,
                              bool is_global);
static void analyze_declarator(Declarator *declarator, Analyze *state);

static void analyze_expr(Tree *ast, Analyze *state);
static void analyze_assign(Tree *ast, Analyze *state);
static void analyze_binary_operator(Tree *ast, Analyze *state);

static void push_lvar_parameter(Tree *arg, Analyze *state);

static void analyze_variable_initialize(Type *var_type, Tree *init_val,
                                        Analyze *state, bool is_global);

static void add_implicit_array_cast(Tree *ast);
static void add_implicit_func_cast(Tree *ast);
static void add_implicit_integer_promotion(Tree *ast);

static void add_cast_stmt(Tree *ast, Type *cast_type);
static void add_arithmetic_conversions(Tree *lhs, Tree *rhs);

static void push_lvar(ObjScope *locals, Obj *lvar);
static Obj *find_lvar(ObjScope *locals, char *lvar_name);
static Obj *find_global(Analyze *state, char *var_name);

static void push_label(LabelScope **lscope, int label_number);
static void pop_label(LabelScope **lscope);

static void push_switch(SwitchScope **switch_scope, Tree *switch_node);
static void pop_switch(SwitchScope **switch_scope);

static StructDef *find_struct(Analyze *state, char *struct_name);
static Member *find_struct_member(StructDef *st_def, char *mem_name);

static UnionDef *find_union(Analyze *state, char *union_name);
static Member *find_union_member(UnionDef *union_def, char *mem_name);

static EnumDef *find_enum(Analyze *state, char *en_name);
static EnumDef *find_enum_in_scope(EnumDef *en_defs, char *en_name);
static EnumVal *find_enum_val(Analyze *state, char *name);
static EnumVal *find_enum_val_in_scope(EnumDef *en_defs, char *name);

Analyze *new_analyze_state(void) {
  Analyze *state = calloc(1, sizeof(Analyze));
  state->glb_obj_dict = new_str_dict();
  state->glb_struct_def_dict = new_str_dict();
  state->glb_union_def_dict = new_str_dict();
  state->glb_typedef_dict = new_str_dict();
  state->label_cnt = 0;
  return state;
}

ObjScope *new_obj_scope(void) {
  ObjScope *scope = calloc(1, sizeof(ObjScope));
  scope->local_obj_dict = new_str_dict();
  scope->local_struct_def_dict = new_str_dict();
  scope->local_union_def_dict = new_str_dict();
  scope->local_typedef_dict = new_str_dict();
  return scope;
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

    push_lvar_scope(state);
    analyze_declarator(ast->declarator, state);

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    char *obj_name = getname_declarator(ast->declarator);

    Obj *func = find_global(state, obj_name);
    if (func) {
      if (func->is_defined)
        error_token(ast->error_token, "redefined: %s", obj_name);
      if (!is_same_type(func->type, obj_type))
        error_token(ast->error_token, "conflict type: %s", obj_name);
    } else {
      func = calloc(1, sizeof(Obj));
      func->obj_name = obj_name;
      func->type = obj_type;
      add_str_dict(state->glb_obj_dict, obj_name, func);
    }

    func->is_defined = true;
    func->is_global = true;

    func->stack_size = 0x0;

    state->current_func = func;

    ast->declarator->def_obj = func;

    Tree *cur = getargs_declarator(ast->declarator);
    while (cur) {
      push_lvar_parameter(cur, state);
      cur = cur->next;
    }

    analyze_stmt(ast->func_body, state);

    // finalize
    func->stack_size = calc_rbp_offset(0, func->stack_size, 8);
    pop_lvar_scope(state);

  } else if (ast->kind == DECLARATION) {

    analyze_decl_spec(ast->decl_specs, state, true);

    if (!ast->declarator) {
      return;
    }

    Type *base_type = gettype_decl_spec(ast->decl_specs);

    for (Declarator *cur = ast->declarator; cur; cur = cur->next) {
      // push ObjScope for func-prototype
      push_lvar_scope(state);
      analyze_declarator(cur, state);
      pop_lvar_scope(state);

      char *obj_name = getname_declarator(cur);
      Type *obj_type = gettype_declarator(cur, base_type);

      // get size for null-size array []
      if (obj_type->kind == ARRAY && get_arr_declarator(cur)->is_null_size) {
        if (!cur->init_expr)
          error("tentative array def must have initialize value");

        if (obj_type->ptr_to->kind == CHAR && cur->init_expr->kind == STR) {
          not_implemented(__func__);
        } else if (cur->init_expr->kind == INITIALIZE_LIST) {
          obj_type->arr_size = 0;
          for (InitializeList *init_list_cur = cur->init_expr->init_list;
               init_list_cur; init_list_cur = init_list_cur->next)
            obj_type->arr_size++;
        } else {
          error("tentative array def must have initialize-list or str-literal");
        }
      }

      if (ast->decl_specs->has_typedef) {
        Typedef *new_def = calloc(1, sizeof(Typedef));
        new_def->name = obj_name;
        new_def->type = obj_type;

        add_str_dict(state->glb_typedef_dict, new_def->name, new_def);
      } else {

        Obj *obj = find_global(state, obj_name);
        if (obj) {
          if (obj->is_defined &&
              (obj_type->kind != FUNC && !ast->decl_specs->has_extern))
            error("redefined: %s", obj_name);
          if (!is_same_type(obj->type, obj_type))
            error("conflict type: %s", obj_name);
        } else {
          obj = calloc(1, sizeof(Obj));
          obj->obj_name = obj_name;
          obj->type = obj_type;
          add_str_dict(state->glb_obj_dict, obj_name, obj);
        }

        obj->is_global = true;

        if (obj_type->kind != FUNC && !ast->decl_specs->has_extern)
          obj->is_defined = true;

        if (cur->init_expr)
          analyze_variable_initialize(obj_type, cur->init_expr, state, true);

        cur->def_obj = obj;
      }
    }

  } else {
    error_token(ast->error_token, "not external_decl");
  }
}

void analyze_decl_spec(DeclSpec *decl_spec, Analyze *state, bool is_global) {
  if (decl_spec->type_spec_kind == TypeSpec_STRUCT) {
    StructDef *st_defs = NULL;

    if (decl_spec->st_spec->st_name)
      st_defs = find_struct(state, decl_spec->st_spec->st_name);

    if (!st_defs) {
      st_defs = calloc(1, sizeof(StructDef));

      if (decl_spec->st_spec->st_name) {
        st_defs->st_name = decl_spec->st_spec->st_name;

        if (is_global)
          add_str_dict(state->glb_struct_def_dict, st_defs->st_name, st_defs);
        else
          add_str_dict(state->locals->local_struct_def_dict, st_defs->st_name,
                       st_defs);
      }
    }

    if (st_defs->is_defined && decl_spec->st_spec->has_decl)
      error("redifine struct");

    if (decl_spec->st_spec->has_decl) {
      st_defs->is_defined = true;
      st_defs->size = 0;
      st_defs->alignment = 1;

      Tree *decl_cur = decl_spec->st_spec->members;
      Member head = {.next = NULL};
      Member *mem_cur = &head;
      while (decl_cur) {
        analyze_decl_spec(decl_cur->decl_specs, state, false);
        Type *base_type = gettype_decl_spec(decl_cur->decl_specs);

        for (Declarator *cur = decl_cur->declarator; cur; cur = cur->next) {
          push_lvar_scope(state);
          analyze_declarator(cur, state);
          pop_lvar_scope(state);

          Type *obj_type = gettype_declarator(cur, base_type);
          char *obj_name = getname_declarator(cur);

          Member *mem = calloc(1, sizeof(Member));

          mem->member_name = obj_name;
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
        }

        decl_cur = decl_cur->next;
      }

      st_defs->members = head.next;

      st_defs->size = (st_defs->size % st_defs->alignment == 0)
                          ? st_defs->size
                          : st_defs->size + st_defs->alignment -
                                st_defs->size % st_defs->alignment;
    }

    decl_spec->st_def = st_defs;
  } else if (decl_spec->type_spec_kind == TypeSpec_UNION) {
    UnionDef *union_def = NULL;

    if (decl_spec->union_spec->union_name)
      union_def = find_union(state, decl_spec->union_spec->union_name);

    if (!union_def) {
      union_def = calloc(1, sizeof(UnionDef));

      if (decl_spec->union_spec->union_name) {
        union_def->union_name = decl_spec->union_spec->union_name;
        if (is_global)
          add_str_dict(state->glb_union_def_dict, union_def->union_name,
                       union_def);
        else
          add_str_dict(state->locals->local_union_def_dict,
                       union_def->union_name, union_def);
      }
    }

    if (union_def->is_defined && decl_spec->union_spec->has_decl)
      error("redefine union");

    if (decl_spec->union_spec->has_decl) {
      union_def->is_defined = true;
      union_def->size = 0;
      union_def->alignment = 1;

      Tree *decl_cur = decl_spec->union_spec->members;
      Member head = {.next = NULL};
      Member *mem_cur = &head;

      while (decl_cur) {
        analyze_decl_spec(decl_cur->decl_specs, state, false);
        Type *obj_type = gettype_decl_spec(decl_cur->decl_specs);

        if (decl_cur->declarator->next)
          not_implemented(__func__);

        push_lvar_scope(state);
        analyze_declarator(decl_cur->declarator, state);
        pop_lvar_scope(state);

        obj_type = gettype_declarator(decl_cur->declarator, obj_type);
        char *obj_name = getname_declarator(decl_cur->declarator);

        Member *mem = calloc(1, sizeof(Member));

        mem->member_name = obj_name;
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

        decl_cur = decl_cur->next;
      }

      union_def->members = head.next;
      union_def->size = (union_def->size % union_def->alignment == 0)
                            ? union_def->size
                            : union_def->size + union_def->alignment -
                                  union_def->size % union_def->alignment;
    }

    decl_spec->union_def = union_def;

  } else if (decl_spec->type_spec_kind == TypeSpec_ENUM) {
    EnumDef *en_def = NULL;

    if (decl_spec->en_spec->en_name)
      en_def = find_enum(state, decl_spec->en_spec->en_name);

    if (!en_def) {
      en_def = calloc(1, sizeof(EnumDef));

      if (decl_spec->en_spec->en_name) {
        if (is_global) {
          en_def->en_name = decl_spec->en_spec->en_name;
          en_def->next = state->glb_enum_defs;
          state->glb_enum_defs = en_def;
        } else {
          en_def->en_name = decl_spec->en_spec->en_name;
          en_def->next = state->locals->local_enum_defs;
          state->locals->local_enum_defs = en_def;
        }
      }
    }

    if (en_def->is_defined && decl_spec->en_spec->has_decl)
      error("redifine enum");

    if (decl_spec->en_spec->has_decl) {
      en_def->is_defined = true;

      en_def->members = decl_spec->en_spec->members;
      int val = 0;
      for (EnumVal *cur = en_def->members; cur; cur = cur->next) {
        if (cur->val_expr) {
          analyze_stmt(cur->val_expr, state);
          if (!is_constexpr_integer(cur->val_expr))
            error_token(cur->val_expr->error_token, "not constexpr");

          val = eval_constexpr_integer(cur->val_expr);
        }
        cur->val = val;
        val++;
      }
    }

    decl_spec->en_def = en_def;
  } else if (decl_spec->type_spec_kind == TypeSpec_TYPEDEF_NAME) {
    decl_spec->defined_type = find_typedef(state, decl_spec->def_name);
  }
}

void analyze_declarator(Declarator *declarator, Analyze *state) {
  if (declarator->nest) {
    push_lvar_scope(state);
    analyze_declarator(declarator->nest, state);
    pop_lvar_scope(state);
  }

  if (declarator->type_suffix_kind == FUNC_DECLARATOR) {

    for (Tree *cur = declarator->args; cur; cur = cur->next) {
      analyze_decl_spec(cur->decl_specs, state, false);

      if (cur->declarator) {
        push_lvar_scope(state);
        analyze_declarator(cur->declarator, state);
        pop_lvar_scope(state);
      }
    }
  } else if (declarator->type_suffix_kind == ARRAY_DECLARATOR) {
    for (ArrayDeclarator *cur = declarator->arr_decl; cur; cur = cur->next) {
      if (!cur->is_null_size)
        analyze_stmt(cur->size_expr, state);
    }
  }
}

void push_lvar_parameter(Tree *ast, Analyze *state) {
  if (ast->kind == DECLARATION) {

    Type *obj_type = gettype_decl_spec(ast->decl_specs);
    obj_type = gettype_declarator(ast->declarator, obj_type);

    if (obj_type->kind == ARRAY)
      obj_type->kind = PTR;

    char *obj_name = getname_declarator(ast->declarator);

    if (ast->decl_specs->has_typedef) {
      error_token(ast->error_token, "typedef not allowed in args");
    } else {

      Obj *lvar = calloc(1, sizeof(Obj));
      lvar->obj_name = obj_name;
      lvar->type = obj_type;
      lvar->nth_arg = ast->nth_arg;
      lvar->rbp_offset =
          calc_rbp_offset(state->current_func->stack_size, type_size(obj_type),
                          type_alignment(obj_type));
      state->current_func->stack_size = lvar->rbp_offset;

      ast->declarator->def_obj = lvar;

      push_lvar(state->locals, lvar);
    }

  } else
    error_token(ast->error_token, "cannot analyze parameter");
}

void analyze_stmt(Tree *ast, Analyze *state) {
  switch (ast->kind) {
  case COMPOUND_STMT: {
    push_lvar_scope(state);

    Tree *cur = ast->stmts;
    while (cur) {
      analyze_stmt(cur, state);
      cur = cur->next;
    }

    pop_lvar_scope(state);
  } break;
  case DECLARATION: {
    analyze_decl_spec(ast->decl_specs, state, false);

    if (!ast->declarator) {
      return;
    }

    Type *base_type = gettype_decl_spec(ast->decl_specs);

    for (Declarator *cur = ast->declarator; cur; cur = cur->next) {
      // push ObjScope
      push_lvar_scope(state);
      analyze_declarator(cur, state);
      pop_lvar_scope(state);

      char *obj_name = getname_declarator(cur);
      Type *obj_type = gettype_declarator(cur, base_type);

      // get size for null-size array []
      if (obj_type->kind == ARRAY && get_arr_declarator(cur)->is_null_size) {
        if (!cur->init_expr)
          error("tentative array def must have initialize value");

        if (obj_type->ptr_to->kind == CHAR && cur->init_expr->kind == STR) {
          not_implemented(__func__);
        } else if (cur->init_expr->kind == INITIALIZE_LIST) {
          obj_type->arr_size = 0;
          for (InitializeList *init_list_cur = cur->init_expr->init_list;
               init_list_cur; init_list_cur = init_list_cur->next)
            obj_type->arr_size++;
        } else {
          error("tentative array def must have initialize-list or str-literal");
        }
      }

      if (ast->decl_specs->has_typedef) {
        Typedef *new_def = calloc(1, sizeof(Typedef));
        new_def->name = obj_name;
        new_def->type = obj_type;

        add_str_dict(state->locals->local_typedef_dict, new_def->name, new_def);
      } else {

        Obj *lvar = calloc(1, sizeof(Obj));
        lvar->obj_name = obj_name;
        lvar->type = obj_type;
        lvar->rbp_offset =
            calc_rbp_offset(state->current_func->stack_size,
                            type_size(obj_type), type_alignment(obj_type));
        state->current_func->stack_size = lvar->rbp_offset;

        cur->def_obj = lvar;

        push_lvar(state->locals, lvar);

        if (cur->init_expr)
          analyze_variable_initialize(obj_type, cur->init_expr, state, false);
      }
    }
  } break;
  case LABEL: {
    int label_len =
        strlen(state->current_func->obj_name) + strlen(ast->label_name) + 1;
    char *label_name = calloc(label_len + 1, sizeof(char));
    snprintf(label_name, label_len + 1, "%s.%s", state->current_func->obj_name,
             ast->label_name);
    ast->label_name = label_name;

    analyze_stmt(ast->lhs, state);
  } break;
  case GOTO: {
    int label_len =
        strlen(state->current_func->obj_name) + strlen(ast->label_name) + 1;
    char *label_name = calloc(label_len + 1, sizeof(char));
    snprintf(label_name, label_len + 1, "%s.%s", state->current_func->obj_name,
             ast->label_name);
    ast->label_name = label_name;
  } break;
  case CASE: {
    analyze_stmt(ast->case_num_node, state);
    int case_num = eval_constexpr_integer(ast->case_num_node);

    ast->case_num = case_num;

    if (!state->switch_stmts)
      error_token(ast->error_token, "not in switch-stmt");

    Case *new_case = calloc(1, sizeof(Case));
    new_case->case_num = case_num;
    new_case->next = state->switch_stmts->switch_node->cases;
    state->switch_stmts->switch_node->cases = new_case;

    ast->label_number = state->switch_stmts->switch_node->label_number;

    analyze_stmt(ast->lhs, state);
  } break;
  case DEFAULT: {
    if (!state->switch_stmts)
      error_token(ast->error_token, "not in switch-stmt");

    if (state->switch_stmts->switch_node->has_default)
      error_token(ast->error_token, "already exist default");

    state->switch_stmts->switch_node->has_default = true;
    ast->label_number = state->switch_stmts->switch_node->label_number;

    analyze_stmt(ast->lhs, state);
  } break;
  case RETURN: {
    if (ast->lhs) {
      analyze_stmt(ast->lhs, state);

      add_implicit_array_cast(ast->lhs);
      add_implicit_func_cast(ast->lhs);

      if (!is_compatible(state->current_func->type->return_type, ast->lhs))
        error_token(ast->error_token, "invalid return type");

      add_cast_stmt(ast->lhs, state->current_func->type->return_type);
    } else {
      if (state->current_func->type->return_type->kind != VOID)
        error_token(ast->error_token, "must return value");
    }
  } break;
  case BREAK: {
    if (!state->break_labels)
      error_token(ast->error_token, "invalid break stmt");

    ast->label_number = state->break_labels->label_number;
  } break;
  case CONTINUE: {
    if (!state->continue_labels)
      error_token(ast->error_token, "invalid continue stmt");

    ast->label_number = state->continue_labels->label_number;
  } break;
  case WHILE: {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    analyze_stmt(ast->cond, state);

    push_label(&state->break_labels, ast->label_number);
    push_label(&state->continue_labels, ast->label_number);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_label(&state->continue_labels);
  } break;
  case DO_WHILE: {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    analyze_stmt(ast->cond, state);

    push_label(&state->break_labels, ast->label_number);
    push_label(&state->continue_labels, ast->label_number);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_label(&state->continue_labels);
  } break;
  case FOR: {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    push_lvar_scope(state);

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

    pop_lvar_scope(state);
  } break;
  case IF: {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);
    analyze_stmt(ast->lhs, state);
    if (ast->rhs)
      analyze_stmt(ast->rhs, state);
  } break;
  case SWITCH: {
    ast->label_number = state->label_cnt;
    state->label_cnt++;
    analyze_stmt(ast->cond, state);

    push_label(&state->break_labels, ast->label_number);
    push_switch(&state->switch_stmts, ast);

    analyze_stmt(ast->lhs, state);

    pop_label(&state->break_labels);
    pop_switch(&state->switch_stmts);
  } break;

    // analyze builtin_function here
  case BUILTIN_VA_START: {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);
    if (ast->lhs->kind != VAR || ast->rhs->kind != VAR)
      error_token(ast->error_token, "invalid usage  __builtin_va_start");
  } break;
  case BUILTIN_VA_END: {
    analyze_stmt(ast->lhs, state);
    if (ast->lhs->kind != VAR)
      error_token(ast->error_token, "invalid usage  __builtin_va_end");
  } break;

    // call analyze_expr
  case COMMA:
  case ASSIGN:
  case ADD_ASSIGN:
  case SUB_ASSIGN:
  case MUL_ASSIGN:
  case DIV_ASSIGN:
  case MOD_ASSIGN:
  case AND_ASSIGN:
  case OR_ASSIGN:
  case XOR_ASSIGN:
  case LSHIFT_ASSIGN:
  case RSHIFT_ASSIGN:
  case CONDITIONAL:
  case LOGICAL_OR:
  case LOGICAL_AND:
  case BIT_OR:
  case BIT_XOR:
  case BIT_AND:
  case EQUAL:
  case NOT_EQUAL:
  case SMALLER:
  case SMALLER_EQUAL:
  case GREATER:
  case GREATER_EQUAL:
  case LSHIFT:
  case RSHIFT:
  case ADD:
  case SUB:
  case MUL:
  case DIV:
  case MOD:
  case CAST:
  case PLUS:
  case MINUS:
  case ADDR:
  case DEREF:
  case LOGICAL_NOT:
  case BIT_NOT:
  case SIZEOF:
  case ALIGNOF:
  case FUNC_CALL:
  case POST_INCREMENT:
  case POST_DECREMENT:
  case DOT:
  case ARROW:
  case NUM:
  case STR:
  case VAR: {
    analyze_expr(ast, state);
  } break;
  default:
    error("invalid ast kind");
  }
}

void analyze_expr(Tree *ast, Analyze *state) {
  switch (ast->kind) {
  case COMMA: {
    analyze_stmt(ast->lhs, state);
    analyze_stmt(ast->rhs, state);

    ast->type = ast->rhs->type;
  } break;

    // assign
  case ASSIGN:
  case ADD_ASSIGN:
  case SUB_ASSIGN:
  case MUL_ASSIGN:
  case DIV_ASSIGN:
  case MOD_ASSIGN:
  case AND_ASSIGN:
  case OR_ASSIGN:
  case XOR_ASSIGN:
  case LSHIFT_ASSIGN:
  case RSHIFT_ASSIGN: {
    analyze_assign(ast, state);
  } break;

  case CONDITIONAL: {
    ast->label_number = state->label_cnt;
    state->label_cnt++;

    analyze_stmt(ast->cond, state);
    if (!is_scalar(ast->cond->type))
      error_token(ast->cond->error_token, "not scalar type");

    analyze_stmt(ast->lhs, state);
    add_implicit_array_cast(ast->lhs);
    add_implicit_func_cast(ast->lhs);
    add_implicit_integer_promotion(ast->lhs);

    analyze_stmt(ast->rhs, state);
    add_implicit_array_cast(ast->rhs);
    add_implicit_func_cast(ast->rhs);
    add_implicit_integer_promotion(ast->rhs);

    // TODO type check
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
      ast->type = ast->lhs->type;
    } else if (ast->lhs->type->kind == STRUCT &&
               is_same_type(ast->lhs->type, ast->rhs->type)) {
      ast->type = ast->lhs->type;
    } else if (ast->lhs->type->kind == UNION &&
               is_same_type(ast->lhs->type, ast->rhs->type)) {
      ast->type = ast->lhs->type;
    } else if (ast->lhs->type->kind == VOID && ast->rhs->type->kind == VOID) {
      ast->type = &type_void;
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      ast->type = ast->lhs->type;
    } else if (is_constexpr_zero(ast->lhs) && ast->rhs->type->kind == PTR) {
      ast->type = ast->rhs->type;
    } else if (ast->lhs->type->kind == PTR && is_constexpr_zero(ast->rhs)) {
      ast->type = ast->lhs->type;
    } else
      error_token(ast->error_token, "invalid type pair");

  } break;

    // binary operator
  case LOGICAL_OR:
  case LOGICAL_AND:
  case BIT_OR:
  case BIT_XOR:
  case BIT_AND:
  case EQUAL:
  case NOT_EQUAL:
  case SMALLER:
  case SMALLER_EQUAL:
  case GREATER:
  case GREATER_EQUAL:
  case LSHIFT:
  case RSHIFT:
  case ADD:
  case SUB:
  case MUL:
  case DIV:
  case MOD: {
    analyze_binary_operator(ast, state);
  } break;

    // cast
  case CAST: {
    analyze_decl_spec(ast->type_name->decl_specs, state, false);
    Type *cast_type = gettype_decl_spec(ast->type_name->decl_specs);
    if (ast->type_name->declarator) {
      push_lvar_scope(state);
      analyze_declarator(ast->type_name->declarator, state);
      pop_lvar_scope(state);

      cast_type = gettype_declarator(ast->type_name->declarator, cast_type);
    }

    analyze_stmt(ast->lhs, state);

    if (!is_compatible(cast_type, ast->lhs))
      error_token(ast->error_token, "cannot cast");

    ast->type = cast_type;
  } break;

    // unary
  case PLUS: {
    analyze_stmt(ast->lhs, state);
    if (!is_arithmetic(ast->lhs->type))
      error_token(ast->lhs->error_token, "not arithmetic type");

    add_implicit_integer_promotion(ast->lhs);
    ast->type = ast->lhs->type;
  } break;
  case MINUS: {
    analyze_stmt(ast->lhs, state);
    if (!is_arithmetic(ast->lhs->type))
      error_token(ast->lhs->error_token, "not arithmetic type");

    add_implicit_integer_promotion(ast->lhs);
    ast->type = ast->lhs->type;
  } break;
  case BIT_NOT: {
    analyze_stmt(ast->lhs, state);
    if (!is_integer(ast->lhs->type))
      error_token(ast->lhs->error_token, "not integer type");

    add_implicit_integer_promotion(ast->lhs);
    ast->type = ast->lhs->type;
  } break;
  case LOGICAL_NOT: {
    analyze_stmt(ast->lhs, state);
    if (!is_scalar(ast->lhs->type))
      error_token(ast->lhs->error_token, "not scalar type");

    ast->type = &type_int;
  } break;
  case ADDR: {
    analyze_stmt(ast->lhs, state);

    if (ast->lhs->type->kind == FUNC)
      ast->type = ast->lhs->type;
    else
      ast->type = newtype_ptr(ast->lhs->type);
  } break;
  case DEREF: {
    analyze_stmt(ast->lhs, state);

    add_implicit_array_cast(ast->lhs);
    add_implicit_func_cast(ast->lhs);

    if (ast->lhs->type->kind != PTR)
      error_token(ast->error_token, "cannot deref");
    ast->type = ast->lhs->type->ptr_to;
  } break;
  case SIZEOF: {
    if (ast->lhs->kind == TYPE_NAME) {
      analyze_decl_spec(ast->lhs->decl_specs, state, false);
      Type *base_type = gettype_decl_spec(ast->lhs->decl_specs);
      if (ast->lhs->declarator) {
        push_lvar_scope(state);
        analyze_declarator(ast->lhs->declarator, state);
        pop_lvar_scope(state);

        base_type = gettype_declarator(ast->lhs->declarator, base_type);
      }

      // replace "sizeof" -> num
      ast->kind = NUM;
      ast->num = type_size(base_type);
      ast->type = &type_int; // TODO size_t in stddef.h
    } else {
      analyze_stmt(ast->lhs, state);

      // replace "sizeof" -> num
      ast->kind = NUM;
      ast->num = type_size(ast->lhs->type);
      ast->type = &type_int;
    }
  } break;
  case ALIGNOF: {
    analyze_decl_spec(ast->lhs->decl_specs, state, false);
    Type *base_type = gettype_decl_spec(ast->lhs->decl_specs);
    if (ast->lhs->declarator) {
      push_lvar_scope(state);
      analyze_declarator(ast->lhs->declarator, state);
      pop_lvar_scope(state);

      base_type = gettype_declarator(ast->lhs->declarator, base_type);
    }

    // replace "sizeof" -> num
    ast->kind = NUM;
    ast->num = type_alignment(base_type);
    ast->type = &type_int; // TODO size_t in stddef.h
  } break;

    // postfix
  case FUNC_CALL: {
    analyze_stmt(ast->lhs, state);
    Type *func_type = NULL;
    if (ast->lhs->type->kind == FUNC) {
      func_type = ast->lhs->type;
    } else if (ast->lhs->type->kind == PTR &&
               ast->lhs->type->ptr_to->kind == FUNC) {
      func_type = ast->lhs->type->ptr_to;
    } else
      error_token(ast->error_token, "cannot call func");

    long argtype_size = size_vector(func_type->args_vector);
    long arg_size = size_vector(ast->call_args_vector);

    if (arg_size < argtype_size)
      error_token(ast->error_token, "less arguments");

    if (func_type->has_arg && !func_type->has_variable_arg &&
        (argtype_size != arg_size))
      error_token(ast->error_token, "excess arguments");

    for (long i = 0; i < arg_size; i++) {
      Tree *arg = get_vector(ast->call_args_vector, i);
      analyze_stmt(arg, state);
      add_implicit_array_cast(arg);
      add_implicit_func_cast(arg);

      if (i < argtype_size) {
        Type *argtype = get_vector(func_type->args_vector, i);
        if (!is_compatible(argtype, arg))
          error_token(arg->error_token, "invalid argtype");

        add_cast_stmt(arg, argtype);
      }
    }

    ast->type = func_type->return_type;
  } break;
  case POST_INCREMENT: {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } break;
  case POST_DECREMENT: {
    analyze_stmt(ast->lhs, state);
    ast->type = ast->lhs->type;
  } break;
  case DOT: {
    analyze_stmt(ast->lhs, state);
    if (ast->lhs->type->kind != STRUCT && ast->lhs->type->kind != UNION)
      error_token(ast->error_token, "lhs is not struct");

    Member *member;
    if (ast->lhs->type->kind == STRUCT)
      member = find_struct_member(ast->lhs->type->st_def, ast->member_name);
    else
      member = find_union_member(ast->lhs->type->union_def, ast->member_name);

    if (!member)
      error_token(ast->error_token, "not find member");

    ast->member = member;
    ast->type = member->type;
  } break;
  case ARROW: {
    analyze_stmt(ast->lhs, state);
    if (ast->lhs->type->kind != PTR ||
        (ast->lhs->type->ptr_to->kind != STRUCT &&
         ast->lhs->type->ptr_to->kind != UNION))
      error_token(ast->error_token, "lhs is not ptr to struct");

    Member *member;
    if (ast->lhs->type->ptr_to->kind == STRUCT)
      member =
          find_struct_member(ast->lhs->type->ptr_to->st_def, ast->member_name);
    else
      member = find_union_member(ast->lhs->type->ptr_to->union_def,
                                 ast->member_name);

    if (!member)
      error_token(ast->error_token, "not find member");

    ast->member = member;
    ast->type = member->type;
  } break;

    // primary
  case NUM: {
    if (ast->is_long)
      ast->type = &type_long;
    else
      ast->type = &type_int;
  } break;
  case STR: {
    ast->type = newtype_ptr(&type_char);
  } break;
  case VAR: {
    // predefined ident
    if (!memcmp(ast->var_name, "__func__", 8)) {

      if (!state->current_func)
        error_token(ast->error_token, "__func__ is not in function");

      // make str-literal for func-name
      StrLiteral *func_name = calloc(1, sizeof(StrLiteral));
      func_name->len = strlen(state->current_func->obj_name);
      func_name->str = calloc(func_name->len + 1, sizeof(char));
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
      ast->type = newtype_ptr(&type_char);
      return;
    }

    EnumVal *en_val = find_enum_val(state, ast->var_name);
    if (en_val) {
      ast->kind = NUM;
      ast->num = en_val->val;
      ast->type = &type_int;
      return;
    }

    Obj *var = NULL;
    if (state->locals)
      var = find_lvar(state->locals, ast->var_name);

    if (!var) {
      var = find_global(state, ast->var_name);
      if (!var)
        error_token(ast->error_token, "cannot find var: %s", ast->var_name);
    }

    ast->var_obj = var;
    ast->type = var->type;
  } break;
  default:
    error("not expr");
  }
}

void analyze_assign(Tree *ast, Analyze *state) {

  analyze_stmt(ast->lhs, state);

  analyze_stmt(ast->rhs, state);
  add_implicit_array_cast(ast->rhs);
  add_implicit_func_cast(ast->rhs);
  add_implicit_integer_promotion(ast->rhs);

  switch (ast->kind) {
  case ASSIGN: {
    add_cast_stmt(ast->rhs, ast->lhs->type);
    ast->type = ast->lhs->type;
  } break;
  case ADD_ASSIGN: {
    if (ast->lhs->type->kind == PTR && is_integer(ast->rhs->type)) {
    } else if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot add-assign");

    ast->type = ast->lhs->type;
  } break;
  case SUB_ASSIGN: {
    if (ast->lhs->type->kind == PTR && is_integer(ast->rhs->type)) {
    } else if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot sub-assign");

    ast->type = ast->lhs->type;
  } break;
  case MUL_ASSIGN: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot mul-assign");

    ast->type = ast->lhs->type;
  } break;
  case DIV_ASSIGN: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot div-assign");

    ast->type = ast->lhs->type;
  } break;
  case MOD_ASSIGN: {
    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot mod-assign");

    ast->type = ast->lhs->type;
  } break;
  case AND_ASSIGN: {
    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot and-assign");

    ast->type = ast->lhs->type;
  } break;
  case OR_ASSIGN: {
    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot or-assign");

    ast->type = ast->lhs->type;
  } break;
  case XOR_ASSIGN: {
    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot xor-assign");

    ast->type = ast->lhs->type;
  } break;
  case LSHIFT_ASSIGN: {
    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot lshift-assign");

    ast->type = ast->lhs->type;
  } break;
  case RSHIFT_ASSIGN: {
    if (is_integer(ast->lhs->type) && is_integer(ast->rhs->type)) {
    } else
      error_token(ast->error_token, "cannot rshift-assign");

    ast->type = ast->lhs->type;
  } break;
  default:
    error("invalid ast kind");
  }
}

void analyze_binary_operator(Tree *ast, Analyze *state) {

  // analyze & cast lhs,rhs

  analyze_stmt(ast->lhs, state);
  add_implicit_array_cast(ast->lhs);
  add_implicit_func_cast(ast->lhs);
  add_implicit_integer_promotion(ast->lhs);

  analyze_stmt(ast->rhs, state);
  add_implicit_array_cast(ast->rhs);
  add_implicit_func_cast(ast->rhs);
  add_implicit_integer_promotion(ast->rhs);

  switch (ast->kind) {
  case LOGICAL_OR: {
    if (!is_scalar(ast->lhs->type) || !is_scalar(ast->rhs->type))
      error_token(ast->error_token, "not scalar type");

    ast->label_number = state->label_cnt;
    state->label_cnt++;
    ast->type = &type_int;
  } break;
  case LOGICAL_AND: {
    if (!is_scalar(ast->lhs->type) || !is_scalar(ast->rhs->type))
      error_token(ast->error_token, "not scalar type");

    ast->label_number = state->label_cnt;
    state->label_cnt++;
    ast->type = &type_int;
  } break;
  case BIT_OR: {
    if (!is_integer(ast->lhs->type) || !is_integer(ast->rhs->type))
      error_token(ast->error_token, "not integer type");

    add_arithmetic_conversions(ast->lhs, ast->rhs);
    ast->type = ast->lhs->type;
  } break;
  case BIT_XOR: {
    if (!is_integer(ast->lhs->type) || !is_integer(ast->rhs->type))
      error_token(ast->error_token, "not integer type");

    add_arithmetic_conversions(ast->lhs, ast->rhs);
    ast->type = ast->lhs->type;
  } break;
  case BIT_AND: {
    if (!is_integer(ast->lhs->type) || !is_integer(ast->rhs->type))
      error_token(ast->error_token, "not integer type");

    add_arithmetic_conversions(ast->lhs, ast->rhs);
    ast->type = ast->lhs->type;
  } break;
  case EQUAL: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      // TODO check compatible ptr
    } else if (ast->lhs->type->kind == PTR && is_constexpr_zero(ast->rhs)) {
    } else if (ast->rhs->type->kind == PTR && is_constexpr_zero(ast->lhs)) {
    } else
      error_token(ast->error_token, "invalid type pair");

    ast->type = &type_int;
  } break;
  case NOT_EQUAL: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      // TODO check compatible ptr
    } else if (ast->lhs->type->kind == PTR && is_constexpr_zero(ast->rhs)) {
    } else if (ast->rhs->type->kind == PTR && is_constexpr_zero(ast->lhs)) {
    } else
      error_token(ast->error_token, "invalid type pair");

    ast->type = &type_int;
  } break;
  case SMALLER: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      // TODO check compatible ptr
    } else
      error_token(ast->error_token, "invalid type pair");

    ast->type = &type_int;
  } break;
  case SMALLER_EQUAL: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      // TODO check compatible ptr
    } else
      error_token(ast->error_token, "invalid type pair");

    ast->type = &type_int;
  } break;
  case GREATER: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      // TODO check compatible ptr
    } else
      error_token(ast->error_token, "invalid type pair");

    ast->type = &type_int;
  } break;
  case GREATER_EQUAL: {
    if (is_arithmetic(ast->lhs->type) && is_arithmetic(ast->rhs->type)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
    } else if (ast->lhs->type->kind == PTR && ast->rhs->type->kind == PTR) {
      // TODO check compatible ptr
    } else
      error_token(ast->error_token, "invalid type pair");

    ast->type = &type_int;
  } break;
  case LSHIFT: {
    if (!is_integer(ast->lhs->type) || !is_integer(ast->rhs->type))
      error_token(ast->error_token, "not integer type");
    ast->type = ast->lhs->type;
  } break;
  case RSHIFT: {
    if (!is_integer(ast->lhs->type) || !is_integer(ast->rhs->type))
      error_token(ast->error_token, "not integer type");
    ast->type = ast->lhs->type;
  } break;
  case ADD: {

    Type *ltype = ast->lhs->type;
    Type *rtype = ast->rhs->type;

    if (is_arithmetic(ltype) && is_arithmetic(rtype)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
      ast->type = ast->lhs->type;
    } else if (ltype->kind == PTR && is_integer(rtype)) {
      add_cast_stmt(ast->rhs, &type_long); // TODO ptrdiff_t in stddef.h
      ast->type = ltype;
    } else if (rtype->kind == PTR && is_integer(ltype)) {
      add_cast_stmt(ast->lhs, &type_long);
      ast->type = rtype;
    } else
      error_token(ast->error_token, "unexpected type pair");
  } break;
  case SUB: {

    Type *ltype = ast->lhs->type;
    Type *rtype = ast->rhs->type;

    if (is_arithmetic(ltype) && is_arithmetic(rtype)) {
      add_arithmetic_conversions(ast->lhs, ast->rhs);
      ast->type = ast->lhs->type;
    } else if (ltype->kind == PTR && is_integer(rtype)) {
      add_cast_stmt(ast->rhs, &type_long); // TODO ptrdiff_t in stddef.h
      ast->type = ltype;
    } else if (rtype->kind == PTR && ltype->kind == PTR) {
      ast->type = &type_long; // TODO ptrdiff_t in stddef.h
    } else
      error_token(ast->error_token, "unexpected type pair");
  } break;
  case MUL: {
    if (!is_arithmetic(ast->lhs->type) || !is_arithmetic(ast->rhs->type))
      error_token(ast->error_token, "not arithmetic type");

    add_arithmetic_conversions(ast->lhs, ast->rhs);
    ast->type = ast->lhs->type;
  } break;
  case DIV: {
    if (!is_arithmetic(ast->lhs->type) || !is_arithmetic(ast->rhs->type))
      error_token(ast->error_token, "not arithmetic type");

    add_arithmetic_conversions(ast->lhs, ast->rhs);
    ast->type = ast->lhs->type;
  } break;
  case MOD: {
    if (!is_integer(ast->lhs->type) || !is_integer(ast->rhs->type))
      error_token(ast->error_token, "not integer type");

    add_arithmetic_conversions(ast->lhs, ast->rhs);
    ast->type = ast->lhs->type;
  } break;
  default:
    error("invalid ast kind");
  }
}

void analyze_variable_initialize(Type *var_type, Tree *init_val, Analyze *state,
                                 bool is_global) {
  if (var_type->kind == ARRAY && var_type->ptr_to->kind == CHAR &&
      init_val->kind == STR) {
    not_implemented("array of char initialize with str-literal");
  } else if (init_val->kind != INITIALIZE_LIST) {
    if (var_type->kind == ARRAY)
      error("array must initialize with str-literal or initialize-list");

    analyze_stmt(init_val, state);

    add_implicit_array_cast(init_val);
    add_implicit_func_cast(init_val);

    if (!is_compatible(var_type, init_val))
      error_token(init_val->error_token, "cannot convert type");

    if (is_global && !is_constexpr(init_val))
      error_token(init_val->error_token, "not constexpr for global initialize");

    add_cast_stmt(init_val, var_type);
  } else if (var_type->kind == ARRAY) {
    if (init_val->kind != INITIALIZE_LIST)
      error_token(init_val->error_token,
                  "must initialize with INITIALIZE_LIST");

    int cnt = 0;
    for (InitializeList *cur = init_val->init_list; cur; cur = cur->next) {
      analyze_variable_initialize(var_type->ptr_to, cur->init_val, state,
                                  is_global);
      cnt++;
    }

    if (cnt > var_type->arr_size)
      error_token(init_val->error_token, "excess elements");
  } else if (var_type->kind == STRUCT) {
    if (init_val->kind != INITIALIZE_LIST)
      error_token(init_val->error_token,
                  "must initialize with INITIALIZE_LIST");

    Member *mem_cur = var_type->st_def->members;
    for (InitializeList *cur = init_val->init_list; cur; cur = cur->next) {
      if (!mem_cur)
        error_token(init_val->error_token, "excess elements");

      if (cur->member_name) {
        while (strcmp(cur->member_name, mem_cur->member_name) != 0) {
          mem_cur = mem_cur->next;
          if (!mem_cur)
            error_token(init_val->error_token,
                        "not found member %s in initialize-list",
                        cur->member_name);
        }
      }

      analyze_variable_initialize(mem_cur->type, cur->init_val, state,
                                  is_global);
      mem_cur = mem_cur->next;
    }
  } else {
    not_implemented(__func__);
  }
}

void add_implicit_array_cast(Tree *ast) {
  Type *arr_type = ast->type;
  if (arr_type->kind != ARRAY)
    return;

  Tree *new_node = calloc(1, sizeof(Tree));
  memcpy(new_node, ast, sizeof(Tree));

  ast->kind = CAST;
  ast->is_implicit = true;
  ast->lhs = new_node;
  ast->type = newtype_ptr(arr_type->ptr_to);
}

void add_implicit_func_cast(Tree *ast) {
  Type *func_type = ast->type;
  if (func_type->kind != FUNC)
    return;

  Tree *new_node = calloc(1, sizeof(Tree));
  memcpy(new_node, ast, sizeof(Tree));

  ast->kind = CAST;
  ast->is_implicit = true;
  ast->lhs = new_node;
  ast->type = newtype_ptr(func_type);
}

void push_lvar_scope(Analyze *state) {
  ObjScope *lsc = new_obj_scope();
  lsc->next = state->locals;
  state->locals = lsc;
}

void add_implicit_integer_promotion(Tree *ast) {
  Type *integer_type = ast->type;
  if (!is_integer(integer_type))
    return;

  if (integer_type->kind == LONG || integer_type->kind == LONGLONG)
    return;

  Tree *new_node = calloc(1, sizeof(Tree));
  memcpy(new_node, ast, sizeof(Tree));

  ast->kind = CAST;
  ast->is_implicit = true;
  ast->lhs = new_node;
  ast->type = &type_int;
}

void add_cast_stmt(Tree *ast, Type *cast_type) {
  if (!is_compatible(cast_type, ast))
    error_token(ast->error_token, "cannot cast");

  Tree *new_node = calloc(1, sizeof(Tree));
  memcpy(new_node, ast, sizeof(Tree));

  ast->kind = CAST;
  ast->is_implicit = true;
  ast->lhs = new_node;
  ast->type = cast_type;
}

void add_arithmetic_conversions(Tree *lhs, Tree *rhs) {
  Type *ltype = lhs->type;
  Type *rtype = rhs->type;

  if (!is_integer(ltype) || !is_integer(rtype))
    error("not integer");

  if (integer_rank(ltype) >= integer_rank(rtype)) {
    Tree *new_node = calloc(1, sizeof(Tree));
    memcpy(new_node, rhs, sizeof(Tree));
    rhs->kind = CAST;
    rhs->is_implicit = true;
    rhs->lhs = new_node;
    rhs->type = ltype;
  } else {
    Tree *new_node = calloc(1, sizeof(Tree));
    memcpy(new_node, lhs, sizeof(Tree));
    lhs->kind = CAST;
    lhs->is_implicit = true;
    lhs->lhs = new_node;
    lhs->type = rtype;
  }
}

void pop_lvar_scope(Analyze *state) { state->locals = state->locals->next; }

void push_lvar(ObjScope *locals, Obj *lvar) {
  if (!lvar->obj_name)
    error("lvar name is null");
  add_str_dict(locals->local_obj_dict, lvar->obj_name, lvar);
}

Obj *find_lvar(ObjScope *locals, char *lvar_name) {
  if (!lvar_name)
    error("lvar name is null");
  for (ObjScope *cur_scope = locals; cur_scope; cur_scope = cur_scope->next)
    if (find_str_dict(cur_scope->local_obj_dict, lvar_name))
      return find_str_dict(cur_scope->local_obj_dict, lvar_name);
  return NULL;
}

Obj *find_global(Analyze *state, char *var_name) {
  return find_str_dict(state->glb_obj_dict, var_name);
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
  for (ObjScope *cur = state->locals; cur; cur = cur->next)
    if (find_str_dict(cur->local_struct_def_dict, struct_name))
      return find_str_dict(cur->local_struct_def_dict, struct_name);
  return find_str_dict(state->glb_struct_def_dict, struct_name);
}

UnionDef *find_union(Analyze *state, char *union_name) {
  for (ObjScope *cur = state->locals; cur; cur = cur->next)
    if (find_str_dict(cur->local_union_def_dict, union_name))
      return find_str_dict(cur->local_union_def_dict, union_name);
  return find_str_dict(state->glb_union_def_dict, union_name);
}

Member *find_struct_member(StructDef *st_def, char *mem_name) {
  for (Member *cur = st_def->members; cur; cur = cur->next)
    if (strcmp(mem_name, cur->member_name) == 0)
      return cur;
  return NULL;
}

Member *find_union_member(UnionDef *union_def, char *mem_name) {
  for (Member *cur = union_def->members; cur; cur = cur->next)
    if (strcmp(mem_name, cur->member_name) == 0)
      return cur;
  return NULL;
}

EnumDef *find_enum(Analyze *state, char *en_name) {
  for (ObjScope *cur = state->locals; cur; cur = cur->next)
    if (find_enum_in_scope(cur->local_enum_defs, en_name))
      return find_enum_in_scope(cur->local_enum_defs, en_name);
  return find_enum_in_scope(state->glb_enum_defs, en_name);
}

EnumDef *find_enum_in_scope(EnumDef *en_defs, char *en_name) {
  for (EnumDef *cur = en_defs; cur; cur = cur->next)
    if (strcmp(en_name, cur->en_name) == 0)
      return cur;
  return NULL;
}

EnumVal *find_enum_val(Analyze *state, char *name) {
  for (ObjScope *cur = state->locals; cur; cur = cur->next)
    if (find_enum_val_in_scope(cur->local_enum_defs, name))
      return find_enum_val_in_scope(cur->local_enum_defs, name);
  return find_enum_val_in_scope(state->glb_enum_defs, name);
}

EnumVal *find_enum_val_in_scope(EnumDef *en_defs, char *name) {
  for (EnumDef *cur_def = en_defs; cur_def; cur_def = cur_def->next)
    for (EnumVal *cur = cur_def->members; cur; cur = cur->next)
      if (strcmp(name, cur->name) == 0)
        return cur;
  return NULL;
}

Typedef *find_typedef(Analyze *state, char *typedef_name) {
  for (ObjScope *cur = state->locals; cur; cur = cur->next)
    if (find_str_dict(cur->local_typedef_dict, typedef_name))
      return find_str_dict(cur->local_typedef_dict, typedef_name);
  return find_str_dict(state->glb_typedef_dict, typedef_name);
}
