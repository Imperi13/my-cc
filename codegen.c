
#include <stdio.h>
#include <string.h>

#include "codegen.h"
#include "codegen_util.h"
#include "constexpr.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void codegen_str_literal(FILE *codegen_output, StrLiteral *sl);
static void codegen_var_definition(FILE *codegen_output, Tree *var);
static void codegen_function(FILE *codegen_output, Tree *func);
static void codegen_stmt(FILE *codegen_output, Tree *stmt);
static void codegen_expr(FILE *codegen_output, Tree *expr);
static void codegen_binary_operator(FILE *codegen_output, Tree *expr);
static void codegen_addr(FILE *codegen_output, Tree *stmt);

static void store2rdiaddr_local_var_initialize(FILE *codegen_output,
                                               Type *var_type, Tree *init_val);

static void load2rax_from_raxaddr(FILE *codegen_output, Type *type);
static void store2rdiaddr_from_rax(FILE *codegen_output, Type *type);

Obj *current_function = NULL;
Vector *str_literal_vector;

void codegen_translation_unit(FILE *codegen_output, Tree *head) {

  int str_literal_cnt = size_vector(str_literal_vector);
  for (int i = 0; i < str_literal_cnt; i++) {
    StrLiteral *now = get_vector(str_literal_vector, i);
    codegen_str_literal(codegen_output, now);
  }

  Tree *cur = head;
  while (cur) {
    if (cur->kind == DECLARATION && cur->declarator &&
        !cur->decl_specs->has_typedef && !cur->decl_specs->has_extern) {
      codegen_var_definition(codegen_output, cur);
    }
    cur = cur->next;
  }

  cur = head;
  while (cur) {
    if (cur->kind == FUNC_DEF) {
      codegen_function(codegen_output, cur);
    }
    cur = cur->next;
  }

  fprintf(codegen_output, "  .section .note.GNU-stack,\"\",@progbits\n");
}

void codegen_str_literal(FILE *codegen_output, StrLiteral *sl) {
  fprintf(codegen_output, "  .local .LC%d\n", sl->id);
  fprintf(codegen_output, "  .data\n");
  fprintf(codegen_output, ".LC%d:\n", sl->id);
  //  fprintf(codegen_output,"  .string \"%.*s\"\n", sl->len, sl->str);
  fprintf(codegen_output, "  .string \"");
  for (int i = 0; i < sl->len; i++) {
    if (sl->str[i] == '\e') {
      fprintf(codegen_output, "\\033");
    } else if (sl->str[i] == '\t') {
      fprintf(codegen_output, "\\e");
    } else if (sl->str[i] == '\n') {
      fprintf(codegen_output, "\\n");
    } else if (sl->str[i] == '\\') {
      fprintf(codegen_output, "\\\\");
    } else if (sl->str[i] == '\'') {
      fprintf(codegen_output, "\\\'");
    } else if (sl->str[i] == '\"') {
      fprintf(codegen_output, "\\\"");
    } else {
      fprintf(codegen_output, "%c", sl->str[i]);
    }
  }
  fprintf(codegen_output, "\"\n");
}

void codegen_var_definition(FILE *codegen_output, Tree *var) {
  for (Declarator *cur = var->declarator; cur; cur = cur->next) {
    Obj *obj = cur->def_obj;

    // ignore function prototype
    if (obj->type->kind == FUNC)
      continue;

    fprintf(codegen_output, "  .globl %s\n", obj->obj_name);

    if (cur->init_expr)
      fprintf(codegen_output, "  .data\n");
    else
      fprintf(codegen_output, "  .bss\n");

    fprintf(codegen_output, "  .align %d\n", type_alignment(obj->type));
    fprintf(codegen_output, "%s:\n", obj->obj_name);

    if (cur->init_expr) {
      codegen_global_initialize(codegen_output, obj->type, cur->init_expr);
    } else
      fprintf(codegen_output, "  .zero %d\n", type_size(obj->type));
  }
}

void codegen_function(FILE *codegen_output, Tree *func) {

  current_function = func->declarator->def_obj;

  fprintf(codegen_output, "  .text\n");
  if (func->decl_specs->has_static)
    fprintf(codegen_output, ".local %s\n", current_function->obj_name);
  else
    fprintf(codegen_output, ".globl %s\n", current_function->obj_name);
  fprintf(codegen_output, "%s:\n", current_function->obj_name);

  fprintf(codegen_output, "  pushq %%rbp\n");
  fprintf(codegen_output, "  movq %%rsp, %%rbp\n");
  fprintf(codegen_output, "  subq $%d, %%rsp\n", current_function->stack_size);

  if (func->declarator->has_variable_arg) {
    for (int i = 5; i >= 0; i--) {
      push_reg(codegen_output, call_register[i], &type_long);
    }
    current_function->saved_argument_offset =
        current_function->stack_size + 0x30;
  }

  Tree *cur = getargs_declarator(func->declarator);
  int count = 0;
  while (cur) {
    Obj *cur_obj = cur->declarator->def_obj;
    if (count < 6) {
      fprintf(codegen_output, "  mov%c %s, -%d(%%rbp)\n",
              get_size_suffix(cur_obj->type),
              get_reg_alias(call_register[count], cur_obj->type),
              cur_obj->rbp_offset);
    } else {
      fprintf(codegen_output, "  mov%c %d(%%rbp), %s\n",
              get_size_suffix(cur_obj->type), 0x10 + 0x8 * (count - 6),
              get_reg_alias(&reg_rax, cur_obj->type));
      fprintf(codegen_output, "  mov%c %s, -%d(%%rbp)\n",
              get_size_suffix(cur_obj->type),
              get_reg_alias(&reg_rax, cur_obj->type), cur_obj->rbp_offset);
    }
    count++;
    cur = cur->next;
  }

  codegen_stmt(codegen_output, func->func_body);

  fprintf(codegen_output, "  movq %%rbp, %%rsp\n");
  fprintf(codegen_output, "  popq %%rbp\n");
  fprintf(codegen_output, "  ret\n");

  current_function = NULL;
}

//
void store2rdiaddr_local_var_initialize(FILE *codegen_output, Type *var_type,
                                        Tree *init_val) {
  if (var_type->kind == ARRAY && var_type->ptr_to->kind == CHAR &&
      init_val->kind == STR) {
    not_implemented(__func__);
  } else if (init_val->kind != INITIALIZE_LIST) {
    fprintf(codegen_output, "  pushq %%rdi\n");
    codegen_stmt(codegen_output, init_val);
    fprintf(codegen_output, "  popq %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, var_type);
  } else if (var_type->kind == ARRAY) {
    int cnt = 0;
    fprintf(codegen_output, "  pushq %%rdi\n");

    for (InitializeList *cur = init_val->init_list; cur; cur = cur->next) {
      fprintf(codegen_output, "  popq %%rdi\n");
      fprintf(codegen_output, "  pushq %%rdi\n");
      fprintf(codegen_output, "  addq $%d, %%rdi\n",
              cnt * type_size(var_type->ptr_to));
      store2rdiaddr_local_var_initialize(codegen_output, var_type->ptr_to,
                                         cur->init_val);
      cnt++;
    }

    fprintf(codegen_output, "  popq %%rdi\n");

  } else if (var_type->kind == STRUCT) {
    fprintf(codegen_output, "  pushq %%rdi\n");

    Member *mem_cur = var_type->st_def->members;

    for (InitializeList *cur = init_val->init_list; cur; cur = cur->next) {
      if (cur->member_name) {
        while (strcmp(cur->member_name, mem_cur->member_name) != 0)
          mem_cur = mem_cur->next;
      }
      fprintf(codegen_output, "  popq %%rdi\n");
      fprintf(codegen_output, "  pushq %%rdi\n");
      fprintf(codegen_output, "  addq $%d, %%rdi\n", mem_cur->offset);
      store2rdiaddr_local_var_initialize(codegen_output, mem_cur->type,
                                         cur->init_val);
      mem_cur = mem_cur->next;
    }

    fprintf(codegen_output, "  popq %%rdi\n");

  } else if (var_type->kind == UNION) {
    not_implemented(__func__);
  } else {
    error("%s", __func__);
  }
}

void codegen_addr(FILE *codegen_output, Tree *stmt) {
  assert(stmt, "stmt is NULL");

  if (stmt->kind == VAR) {
    if (!stmt->var_obj->is_global)
      fprintf(codegen_output, "  leaq -%d(%%rbp), %%rax\n",
              stmt->var_obj->rbp_offset);
    else if (stmt->var_obj->is_defined)
      fprintf(codegen_output, "  leaq %s(%%rip), %%rax\n",
              stmt->var_obj->obj_name);
    else
      fprintf(codegen_output, "  movq %s@GOTPCREL(%%rip), %%rax\n",
              stmt->var_obj->obj_name);
  } else if (stmt->kind == DEREF) {
    codegen_stmt(codegen_output, stmt->lhs);
  } else if (stmt->kind == DOT) {
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  addq $%d, %%rax\n", stmt->member->offset);
  } else if (stmt->kind == ARROW) {
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  addq $%d, %%rax\n", stmt->member->offset);
  } else {
    not_implemented(__func__);
  }
}

void codegen_stmt(FILE *codegen_output, Tree *stmt) {
  assert(stmt, "stmt is NULL");

  switch (stmt->kind) {
  case DECLARATION: {
    for (Declarator *cur = stmt->declarator; cur; cur = cur->next) {
      if (cur && cur->init_expr) {
        fprintf(codegen_output, "  leaq -%d(%%rbp), %%rdi\n",
                cur->def_obj->rbp_offset);
        store2rdiaddr_local_var_initialize(codegen_output, cur->def_obj->type,
                                           cur->init_expr);
      }
    }
  } break;
  case LABEL: {
    fprintf(codegen_output, ".Llabel.%s:\n", stmt->label_name);
    codegen_stmt(codegen_output, stmt->lhs);
  } break;
  case GOTO: {
    fprintf(codegen_output, "  jmp .Llabel.%s\n", stmt->label_name);
  } break;
  case CASE: {
    fprintf(codegen_output, ".Lswitch%d_case%d:\n", stmt->label_number,
            stmt->case_num);
    codegen_stmt(codegen_output, stmt->lhs);
  } break;
  case DEFAULT: {
    fprintf(codegen_output, ".Lswitch%d_default:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
  } break;
  case RETURN: {
    if (stmt->lhs)
      codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  movq %%rbp, %%rsp\n");
    fprintf(codegen_output, "  popq %%rbp\n");
    fprintf(codegen_output, "  ret\n");
  } break;
  case BREAK: {
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
  } break;
  case CONTINUE: {
    fprintf(codegen_output, "  jmp .Lloopend%d\n", stmt->label_number);
  } break;
  case COMPOUND_STMT: {
    Tree *cur = stmt->stmts;
    while (cur) {
      codegen_stmt(codegen_output, cur);
      cur = cur->next;
    }
  } break;
  case WHILE: {
    fprintf(codegen_output, ".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(stmt->cond->type),
            get_reg_alias(&reg_rax, stmt->cond->type));
    fprintf(codegen_output, "  je .Lend%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lloopend%d:\n", stmt->label_number);
    fprintf(codegen_output, "  jmp .Lbegin%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
  } break;
  case DO_WHILE: {
    fprintf(codegen_output, ".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lloopend%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(stmt->cond->type),
            get_reg_alias(&reg_rax, stmt->cond->type));
    fprintf(codegen_output, "  jne .Lbegin%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
  } break;
  case FOR: {
    if (stmt->for_init)
      codegen_stmt(codegen_output, stmt->for_init);
    fprintf(codegen_output, ".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(stmt->cond->type),
            get_reg_alias(&reg_rax, stmt->cond->type));
    fprintf(codegen_output, "  je .Lend%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lloopend%d:\n", stmt->label_number);
    if (stmt->for_update)
      codegen_stmt(codegen_output, stmt->for_update);
    fprintf(codegen_output, "  jmp .Lbegin%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
  } break;
  case IF: {
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(stmt->cond->type),
            get_reg_alias(&reg_rax, stmt->cond->type));
    fprintf(codegen_output, "  je .Lelse%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lelse%d:\n", stmt->label_number);
    if (stmt->rhs)
      codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
  } break;
  case SWITCH: {
    codegen_stmt(codegen_output, stmt->cond);
    for (Case *cur = stmt->cases; cur; cur = cur->next) {
      fprintf(codegen_output, "  cmp%c $%d, %s\n",
              get_size_suffix(stmt->cond->type), cur->case_num,
              get_reg_alias(&reg_rax, stmt->cond->type));
      fprintf(codegen_output, "  je .Lswitch%d_case%d\n", stmt->label_number,
              cur->case_num);
    }
    if (stmt->has_default)
      fprintf(codegen_output, "  jmp .Lswitch%d_default\n", stmt->label_number);
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
  } break;

    // codegen builtin_function here
  case BUILTIN_VA_START: {
    Obj *va_obj = stmt->lhs->var_obj;
    Obj *last_para = stmt->rhs->var_obj;

    fprintf(codegen_output, "  movl $%d, %%eax\n", last_para->nth_arg * 0x8);
    fprintf(codegen_output, "  movl %%eax, -%d(%%rbp)\n", va_obj->rbp_offset);
    // not use float,so fix 0x30
    fprintf(codegen_output, "  movl $0x30, %%eax\n");
    fprintf(codegen_output, "  movl %%eax, -%d(%%rbp)\n",
            va_obj->rbp_offset - 0x4);
    fprintf(codegen_output, "  leaq 0x10(%%rbp), %%rax\n");
    fprintf(codegen_output, "  movq %%rax, -%d(%%rbp)\n",
            va_obj->rbp_offset - 0x8);
    fprintf(codegen_output, "  leaq -%d(%%rbp), %%rax\n",
            current_function->saved_argument_offset);
    fprintf(codegen_output, "  movq %%rax, -%d(%%rbp)\n",
            va_obj->rbp_offset - 0x10);
  } break;
  case BUILTIN_VA_END: {
    Obj *va_obj = stmt->lhs->var_obj;
    fprintf(codegen_output, "  movq $0x0, %%rax\n");
    fprintf(codegen_output, "  movq %%rax, -%d(%%rbp)\n", va_obj->rbp_offset);
    fprintf(codegen_output, "  movq %%rax, -%d(%%rbp)\n",
            va_obj->rbp_offset - 0x8);
    fprintf(codegen_output, "  movq %%rax, -%d(%%rbp)\n",
            va_obj->rbp_offset - 0x10);
  } break;

    // expr

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
    codegen_expr(codegen_output, stmt);
  } break;

  default:
    error("invalid ast kind");
  }
}

void codegen_expr(FILE *codegen_output, Tree *expr) {
  assert(expr, "expr is NULL");

  switch (expr->kind) {
  case ASSIGN: {
    assert(is_same_type(expr->lhs->type, expr->rhs->type),
           "not same type on ASSIGN");

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  popq %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;

  case ADD_ASSIGN: {

    if (expr->lhs->type->kind == PTR) {
      codegen_addr(codegen_output, expr->lhs);
      fprintf(codegen_output, "  pushq %%rax\n");
      codegen_stmt(codegen_output, expr->rhs);
      mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
      fprintf(codegen_output, "  popq %%rsi\n");
      fprintf(codegen_output, "  movq %%rsi, %%rax\n");
      load2rax_from_raxaddr(codegen_output, expr->lhs->type);

      // lhs value: rax , rhs value: rdi
      reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, &type_long);
      fprintf(codegen_output, "  imulq $%d, %%rdi\n",
              type_size(expr->lhs->type->ptr_to));
      fprintf(codegen_output, "  addq %%rdi, %%rax\n");

      fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
      store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    } else {
      // arithmetic ADD_ASSIGN

      Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
      Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
      Type *result_type =
          get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

      codegen_addr(codegen_output, expr->lhs);
      fprintf(codegen_output, "  pushq %%rax\n");
      codegen_stmt(codegen_output, expr->rhs);
      mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
      fprintf(codegen_output, "  popq %%rsi\n");
      fprintf(codegen_output, "  movq %%rsi, %%rax\n");
      load2rax_from_raxaddr(codegen_output, expr->lhs->type);

      // lhs value: rax , rhs value: rdi
      reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type,
                       promoted_ltype);
      reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type,
                       promoted_rtype);
      reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
      reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

      fprintf(codegen_output, "  add%c %s, %s\n", get_size_suffix(result_type),
              get_reg_alias(&reg_rdi, result_type),
              get_reg_alias(&reg_rax, result_type));

      reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
      fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
      store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    }
  } break;
  case SUB_ASSIGN: {
    if (expr->lhs->type->kind == PTR) {
      codegen_addr(codegen_output, expr->lhs);
      fprintf(codegen_output, "  pushq %%rax\n");
      codegen_stmt(codegen_output, expr->rhs);
      mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
      fprintf(codegen_output, "  popq %%rsi\n");
      fprintf(codegen_output, "  movq %%rsi, %%rax\n");
      load2rax_from_raxaddr(codegen_output, expr->lhs->type);

      // lhs value: rax , rhs value: rdi
      reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, &type_long);
      fprintf(codegen_output, "  imulq $%d, %%rdi\n",
              type_size(expr->lhs->type->ptr_to));
      fprintf(codegen_output, "  subq %%rdi, %%rax\n");

      fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
      store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    } else {
      // arithmetic SUB_ASSIGN

      Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
      Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
      Type *result_type =
          get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

      codegen_addr(codegen_output, expr->lhs);
      fprintf(codegen_output, "  pushq %%rax\n");
      codegen_stmt(codegen_output, expr->rhs);
      mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
      fprintf(codegen_output, "  popq %%rsi\n");
      fprintf(codegen_output, "  movq %%rsi, %%rax\n");
      load2rax_from_raxaddr(codegen_output, expr->lhs->type);

      // lhs value: rax , rhs value: rdi
      reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type,
                       promoted_ltype);
      reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type,
                       promoted_rtype);
      reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
      reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

      fprintf(codegen_output, "  sub%c %s, %s\n", get_size_suffix(result_type),
              get_reg_alias(&reg_rdi, result_type),
              get_reg_alias(&reg_rax, result_type));

      reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
      fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
      store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    }
  } break;
  case MUL_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
    Type *result_type =
        get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);
    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
    reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

    mul_reg(codegen_output, result_type);

    reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case DIV_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
    Type *result_type =
        get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);
    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
    reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

    div_reg(codegen_output, result_type);

    reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case MOD_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
    Type *result_type =
        get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);
    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
    reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

    mod_reg(codegen_output, result_type);

    reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case AND_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
    Type *result_type =
        get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);
    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
    reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

    fprintf(codegen_output, "  and%c %s, %s\n", get_size_suffix(result_type),
            get_reg_alias(&reg_rdi, result_type),
            get_reg_alias(&reg_rax, result_type));

    reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case OR_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
    Type *result_type =
        get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);
    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
    reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

    fprintf(codegen_output, "  or%c %s, %s\n", get_size_suffix(result_type),
            get_reg_alias(&reg_rdi, result_type),
            get_reg_alias(&reg_rax, result_type));

    reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case XOR_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);
    Type *result_type =
        get_arithmetic_converted_type(promoted_ltype, promoted_rtype);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);
    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, result_type);
    reg_integer_cast(codegen_output, &reg_rdi, promoted_rtype, result_type);

    fprintf(codegen_output, "  xor%c %s, %s\n", get_size_suffix(result_type),
            get_reg_alias(&reg_rdi, result_type),
            get_reg_alias(&reg_rax, result_type));

    reg_integer_cast(codegen_output, &reg_rax, result_type, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case LSHIFT_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);

    lshift_reg(codegen_output, promoted_ltype, promoted_rtype);

    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case RSHIFT_ASSIGN: {
    Type *promoted_ltype = get_integer_promoted_type(expr->lhs->type);
    Type *promoted_rtype = get_integer_promoted_type(expr->rhs->type);

    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  pushq %%rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    fprintf(codegen_output, "  popq %%rsi\n");
    fprintf(codegen_output, "  movq %%rsi, %%rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);

    // lhs value: rax , rhs value: rdi
    reg_integer_cast(codegen_output, &reg_rax, expr->lhs->type, promoted_ltype);
    reg_integer_cast(codegen_output, &reg_rdi, expr->rhs->type, promoted_rtype);

    rshift_reg(codegen_output, promoted_ltype, promoted_rtype);

    reg_integer_cast(codegen_output, &reg_rax, promoted_ltype, expr->lhs->type);
    fprintf(codegen_output, "  movq %%rsi, %%rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
  } break;
  case CONDITIONAL: {
    codegen_stmt(codegen_output, expr->cond);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(expr->cond->type),
            get_reg_alias(&reg_rax, expr->cond->type));
    fprintf(codegen_output, "  jne .Ltrue%d\n", expr->label_number);
    fprintf(codegen_output, "  jmp .Lfalse%d\n", expr->label_number);
    fprintf(codegen_output, ".Ltrue%d:\n", expr->label_number);
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  jmp .Lend%d\n", expr->label_number);
    fprintf(codegen_output, ".Lfalse%d:\n", expr->label_number);
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, ".Lend%d:\n", expr->label_number);
  } break;
  case COMMA: {
    codegen_stmt(codegen_output, expr->lhs);
    codegen_stmt(codegen_output, expr->rhs);
  } break;
  case LOGICAL_OR: {
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(expr->lhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
    fprintf(codegen_output, "  jne .Ltrue%d\n", expr->label_number);
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(expr->rhs->type),
            get_reg_alias(&reg_rax, expr->rhs->type));
    fprintf(codegen_output, "  jne .Ltrue%d\n", expr->label_number);
    mov_imm(codegen_output, &reg_rax, &type_int, 0);
    fprintf(codegen_output, "  jmp .Lend%d\n", expr->label_number);
    fprintf(codegen_output, ".Ltrue%d:\n", expr->label_number);
    mov_imm(codegen_output, &reg_rax, &type_int, 1);
    fprintf(codegen_output, ".Lend%d:\n", expr->label_number);
  } break;
  case LOGICAL_AND: {
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(expr->lhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
    fprintf(codegen_output, "  je .Lfalse%d\n", expr->label_number);
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(expr->rhs->type),
            get_reg_alias(&reg_rax, expr->rhs->type));
    fprintf(codegen_output, "  je .Lfalse%d\n", expr->label_number);
    mov_imm(codegen_output, &reg_rax, &type_int, 1);
    fprintf(codegen_output, "  jmp .Lend%d\n", expr->label_number);
    fprintf(codegen_output, ".Lfalse%d:\n", expr->label_number);
    mov_imm(codegen_output, &reg_rax, &type_int, 0);
    fprintf(codegen_output, ".Lend%d:\n", expr->label_number);
  } break;
  case LOGICAL_NOT: {
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  cmp%c $0, %s\n",
            get_size_suffix(expr->lhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
    fprintf(codegen_output, "  sete %%al\n");
    fprintf(codegen_output, "  movzbl %%al, %%eax\n");
  } break;
  case BIT_NOT: {
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  not%c %s\n", get_size_suffix(expr->lhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
  } break;
  case CAST: {

    // const zero cast
    if (expr->type->kind == PTR && is_constexpr_zero(expr->lhs)) {
      fprintf(codegen_output, "  movq $0, %%rax\n");
      break;
    }

    codegen_stmt(codegen_output, expr->lhs);

    // do nothing for implicit ARRAY,FUNC cast
    if (expr->type->kind == PTR &&
        (expr->lhs->type->kind == ARRAY || expr->lhs->type->kind == FUNC))
      break;

    if (expr->type->kind == BOOL) {
      if (is_scalar(expr->lhs->type)) {
        fprintf(codegen_output, "  cmp%c $0, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
        fprintf(codegen_output, "  setne %%al\n");
      } else
        not_implemented(__func__);
      return;
    }

    // normal cast
    if (is_arithmetic(expr->type) && is_arithmetic(expr->lhs->type)) {
      Register *src_reg = is_integer(expr->lhs->type) ? &reg_rax : &reg_xmm0;
      Register *dst_reg = is_integer(expr->type) ? &reg_rax : &reg_xmm0;
      reg_arithmetic_cast(codegen_output, src_reg, dst_reg, expr->lhs->type,
                          expr->type);
    }

  } break;
  case PLUS: {
    codegen_stmt(codegen_output, expr->lhs);
  } break;
  case MINUS: {
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  neg%c %s\n", get_size_suffix(expr->lhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
  } break;
  case ADDR: {
    codegen_addr(codegen_output, expr->lhs);
  } break;
  case DEREF: {
    codegen_stmt(codegen_output, expr->lhs);
    if (expr->type->kind == ARRAY || expr->type->kind == STRUCT ||
        expr->type->kind == UNION || expr->type->kind == FUNC)
      break;
    load2rax_from_raxaddr(codegen_output, expr->type);
  } break;
  case FUNC_CALL: {
    fprintf(codegen_output, "  movq %%rsp, %%r10\n");
    fprintf(codegen_output, "  andq $0xfffffffffffffff0, %%rsp\n");
    fprintf(codegen_output, "  pushq %%r10\n");
    fprintf(codegen_output, "  pushq $0\n");

    long call_arg_size = size_vector(expr->call_args_vector);

    bool need_padding = (call_arg_size > 6) && (call_arg_size % 2 == 1);
    if (need_padding)
      fprintf(codegen_output, "  pushq $0\n");

    for (long i = call_arg_size - 1; i >= 0; i--) {
      Tree *arg = get_vector(expr->call_args_vector, i);
      codegen_stmt(codegen_output, arg);
      push_reg(codegen_output, &reg_rax, arg->type);
    }

    codegen_stmt(codegen_output, expr->lhs);

    for (int i = 0; i < ((call_arg_size > 6) ? 6 : call_arg_size); i++) {
      Tree *arg = get_vector(expr->call_args_vector, i);
      pop_reg(codegen_output, call_register[i], arg->type);
    }

    fprintf(codegen_output, "  call *%%rax\n");

    // clean stack_arg
    for (int i = 0; i < ((call_arg_size > 6) ? call_arg_size - 6 : 0); i++)
      fprintf(codegen_output, " popq %%r10\n");

    if (need_padding)
      fprintf(codegen_output, "  popq %%r10\n");

    fprintf(codegen_output, "  popq %%r10\n");
    fprintf(codegen_output, "  popq %%rsp\n");
  } break;
  case POST_INCREMENT: {
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  movq %%rax, %%rdi\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);
    push_reg(codegen_output, &reg_rax, expr->lhs->type);
    if (expr->lhs->type->kind == PTR)
      fprintf(codegen_output, "  add%c $%d, %s\n",
              get_size_suffix(expr->lhs->type),
              type_size(expr->lhs->type->ptr_to),
              get_reg_alias(&reg_rax, expr->lhs->type));
    else
      fprintf(codegen_output, "  add%c $1, %s\n",
              get_size_suffix(expr->lhs->type),
              get_reg_alias(&reg_rax, expr->lhs->type));
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    pop_reg(codegen_output, &reg_rax, expr->lhs->type);
  } break;
  case POST_DECREMENT: {
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  movq %%rax, %%rdi\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);
    push_reg(codegen_output, &reg_rax, expr->lhs->type);
    if (expr->lhs->type->kind == PTR)
      fprintf(codegen_output, "  sub%c $%d, %s\n",
              get_size_suffix(expr->lhs->type),
              type_size(expr->lhs->type->ptr_to),
              get_reg_alias(&reg_rax, expr->lhs->type));
    else
      fprintf(codegen_output, "  sub%c $1, %s\n",
              get_size_suffix(expr->lhs->type),
              get_reg_alias(&reg_rax, expr->lhs->type));
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    pop_reg(codegen_output, &reg_rax, expr->lhs->type);
  } break;
  case DOT: {
    codegen_addr(codegen_output, expr);
    if (expr->type->kind == ARRAY || expr->type->kind == STRUCT ||
        expr->type->kind == UNION)
      break;
    load2rax_from_raxaddr(codegen_output, expr->type);
  } break;
  case ARROW: {
    codegen_addr(codegen_output, expr);
    if (expr->type->kind == ARRAY || expr->type->kind == STRUCT ||
        expr->type->kind == UNION)
      break;
    load2rax_from_raxaddr(codegen_output, expr->type);
  } break;
  case NUM: {
    mov_imm(codegen_output, &reg_rax, expr->type, expr->num);
  } break;
  case STR: {
    fprintf(codegen_output, "  leaq .LC%d(%%rip), %%rax\n",
            expr->str_literal->id);
  } break;
  case VAR: {
    codegen_addr(codegen_output, expr);
    if (expr->type->kind == FUNC || expr->type->kind == ARRAY ||
        expr->type->kind == STRUCT || expr->type->kind == UNION)
      break;
    load2rax_from_raxaddr(codegen_output, expr->type);
  } break;

    // binary_op

  case BIT_AND:
  case BIT_XOR:
  case BIT_OR:
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
    codegen_binary_operator(codegen_output, expr);
  } break;

  default:
    error("not expr");
  }
}

void codegen_binary_operator(FILE *codegen_output, Tree *expr) {
  assert(expr, "expr is NULL");
  assert(expr->lhs && expr->rhs, "not binary operator");

  // load lhs(rhs) value to register
  // integer    lhs: rax rhs: rdi
  // floating point lhs: xmm0 rhs: xmm1

  if (is_floating_point(expr->lhs->type)) {
    assert(is_floating_point(expr->rhs->type), "rhs is not floating-type");
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  subq $8, %%rsp\n");
    fprintf(codegen_output, "  movs%c %%xmm0, 0(%%rsp)\n",
            get_floating_point_suffix(expr->lhs->type));

    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  movs%c %%xmm0, %%xmm1\n",
            get_floating_point_suffix(expr->rhs->type));
    fprintf(codegen_output, "  movs%c 0(%%rsp), %%xmm0\n",
            get_floating_point_suffix(expr->lhs->type));
    fprintf(codegen_output, "  addq $8, %%rsp\n");
  } else {
    codegen_stmt(codegen_output, expr->lhs);
    push_reg(codegen_output, &reg_rax, expr->lhs->type);
    codegen_stmt(codegen_output, expr->rhs);
    mov_reg(codegen_output, &reg_rax, &reg_rdi, expr->rhs->type);
    pop_reg(codegen_output, &reg_rax, expr->lhs->type);
  }

  switch (expr->kind) {
  case BIT_AND: {
    assert(is_same_type(expr->lhs->type, expr->rhs->type),
           "not same type on BIT_AND");
    fprintf(codegen_output, "  and%c %s, %s\n", get_size_suffix(expr->type),
            get_reg_alias(&reg_rdi, expr->rhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
  } break;
  case BIT_XOR: {
    assert(is_same_type(expr->lhs->type, expr->rhs->type),
           "not same type on BIT_XOR");
    fprintf(codegen_output, "  xor%c %s, %s\n", get_size_suffix(expr->type),
            get_reg_alias(&reg_rdi, expr->rhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
  } break;
  case BIT_OR: {
    assert(is_same_type(expr->lhs->type, expr->rhs->type),
           "not same type on BIT_OR");
    fprintf(codegen_output, "  or%c %s, %s\n", get_size_suffix(expr->type),
            get_reg_alias(&reg_rdi, expr->rhs->type),
            get_reg_alias(&reg_rax, expr->lhs->type));
  } break;
  case EQUAL: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on EQUAL");

      if (is_scalar(expr->lhs->type))
        fprintf(codegen_output, "  cmp%c %s, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, " comis%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("EQUAL");

      fprintf(codegen_output, "  sete %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    } else {
      fprintf(codegen_output, "  cmpq %%rdi, %%rax\n");
      fprintf(codegen_output, "  sete %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    }
  } break;
  case NOT_EQUAL: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on NOT_EQUAL");

      if (is_scalar(expr->lhs->type))
        fprintf(codegen_output, "  cmp%c %s, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, " comis%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("NOT_EQUAL");

      fprintf(codegen_output, "  setne %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    } else {
      fprintf(codegen_output, "  cmpq %%rdi, %%rax\n");
      fprintf(codegen_output, "  setne %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    }
  } break;
  case SMALLER: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on SMALLER");
      if (is_scalar(expr->lhs->type))
        fprintf(codegen_output, "  cmp%c %s, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, " comis%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("SMALLER");

      if (expr->lhs->type->is_unsigned || is_floating_point(expr->lhs->type))
        fprintf(codegen_output, "  setb %%al\n");
      else
        fprintf(codegen_output, "  setl %%al\n");

      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    } else {
      fprintf(codegen_output, "  cmpq %%rdi, %%rax\n");
      fprintf(codegen_output, "  setl %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    }
  } break;
  case SMALLER_EQUAL: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on SMALLER_EQUAL");

      if (is_scalar(expr->lhs->type))
        fprintf(codegen_output, "  cmp%c %s, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, " comis%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("SMALLER_EQUAL");

      if (expr->lhs->type->is_unsigned || is_floating_point(expr->lhs->type))
        fprintf(codegen_output, "  setna %%al\n");
      else
        fprintf(codegen_output, "  setle %%al\n");

      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    } else {
      fprintf(codegen_output, "  cmpq %%rdi, %%rax\n");
      fprintf(codegen_output, "  setle %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    }
  } break;
  case GREATER: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on GREATER");

      if (is_scalar(expr->lhs->type))
        fprintf(codegen_output, "  cmp%c %s, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, " comis%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("GREATER");

      if (expr->lhs->type->is_unsigned || is_floating_point(expr->lhs->type))
        fprintf(codegen_output, "  seta %%al\n");
      else
        fprintf(codegen_output, "  setg %%al\n");

      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    } else {
      fprintf(codegen_output, "  cmpq %%rdi, %%rax\n");
      fprintf(codegen_output, "  setg %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    }
  } break;
  case GREATER_EQUAL: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on GREATER_EQUAL");

      if (is_scalar(expr->lhs->type))
        fprintf(codegen_output, "  cmp%c %s, %s\n",
                get_size_suffix(expr->lhs->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, " comis%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("GREATER_EQUAL");

      if (expr->lhs->type->is_unsigned || is_floating_point(expr->lhs->type))
        fprintf(codegen_output, "  setnb %%al\n");
      else
        fprintf(codegen_output, "  setge %%al\n");

      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    } else {
      fprintf(codegen_output, "  cmpq %%rdi, %%rax\n");
      fprintf(codegen_output, "  setge %%al\n");
      fprintf(codegen_output, "  movzbl %%al, %%eax\n");
    }
  } break;
  case LSHIFT: {
    lshift_reg(codegen_output, expr->lhs->type, expr->rhs->type);
  } break;
  case RSHIFT: {
    rshift_reg(codegen_output, expr->lhs->type, expr->rhs->type);
  } break;
  case ADD: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on arithmetic ADD");
      if (is_integer(expr->lhs->type))
        fprintf(codegen_output, "  add%c %s, %s\n", get_size_suffix(expr->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, "  adds%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("ADD");
    } else if (expr->lhs->type->kind == PTR) {
      fprintf(codegen_output, "  imulq $%d, %%rdi\n",
              type_size(expr->lhs->type->ptr_to));
      fprintf(codegen_output, "  addq %%rdi, %%rax\n");
    } else if (expr->rhs->type->kind == PTR) {
      fprintf(codegen_output, "  imulq $%d, %%rax\n",
              type_size(expr->rhs->type->ptr_to));
      fprintf(codegen_output, "  addq %%rdi, %%rax\n");
    } else
      error("invalid type pair");
  } break;
  case SUB: {
    if (is_arithmetic(expr->lhs->type) && is_arithmetic(expr->rhs->type)) {
      assert(is_same_type(expr->lhs->type, expr->rhs->type),
             "not same type on arithmetic SUB");

      if (is_integer(expr->lhs->type))
        fprintf(codegen_output, "  sub%c %s, %s\n", get_size_suffix(expr->type),
                get_reg_alias(&reg_rdi, expr->rhs->type),
                get_reg_alias(&reg_rax, expr->lhs->type));
      else if (is_floating_point(expr->lhs->type))
        fprintf(codegen_output, "  subs%c %%xmm1, %%xmm0\n",
                get_floating_point_suffix(expr->lhs->type));
      else
        error("SUB");
    } else if (expr->lhs->type->kind == PTR && is_integer(expr->rhs->type)) {
      fprintf(codegen_output, "  imulq $%d, %%rdi\n",
              type_size(expr->lhs->type->ptr_to));
      fprintf(codegen_output, "  subq %%rdi, %%rax\n");
    } else if (expr->lhs->type->kind == PTR && expr->rhs->type->kind == PTR) {
      fprintf(codegen_output, "  subq %%rdi, %%rax\n");
      fprintf(codegen_output, "  movq $%d, %%rdi\n",
              type_size(expr->lhs->type->ptr_to));
      fprintf(codegen_output, "  cqto\n");
      fprintf(codegen_output, "  idivq %%rdi\n");
    } else
      error("invalid type pair");
  } break;
  case MUL: {
    assert(is_same_type(expr->lhs->type, expr->rhs->type),
           "not same type on MUL");

    if (is_integer(expr->lhs->type))
      mul_reg(codegen_output, expr->lhs->type);
    else if (is_floating_point(expr->lhs->type))
      fprintf(codegen_output, "  muls%c %%xmm1, %%xmm0\n",
              get_floating_point_suffix(expr->lhs->type));
    else
      error("MUL");
  } break;
  case DIV: {
    assert(is_same_type(expr->lhs->type, expr->rhs->type),
           "not same type on DIV");

    if (is_integer(expr->lhs->type))
      div_reg(codegen_output, expr->type);
    else if (is_floating_point(expr->lhs->type))
      fprintf(codegen_output, "  divs%c %%xmm1, %%xmm0\n",
              get_floating_point_suffix(expr->lhs->type));
    else
      error("DIV");
  } break;
  case MOD: {
    mod_reg(codegen_output, expr->type);
  } break;

  default:
    error("cannnot codegen binary_op");
  }
}

// raxレジスタで指しているアドレスからtype型の値をrax,xmm0にロードする
void load2rax_from_raxaddr(FILE *codegen_output, Type *type) {
  if (is_scalar(type))
    fprintf(codegen_output, "  mov%c (%%rax), %s\n", get_size_suffix(type),
            get_reg_alias(&reg_rax, type));
  else if (is_floating_point(type))
    fprintf(codegen_output, "  movs%c (%%rax), %%xmm0\n",
            get_floating_point_suffix(type));
  else
    error("invalid type");
}

// rax ,xmm0レジスタが表すtype型の値をrdiレジスタが指すアドレスにstoreする
void store2rdiaddr_from_rax(FILE *codegen_output, Type *type) {
  if (is_scalar(type)) {
    fprintf(codegen_output, "  mov%c %s, (%%rdi)\n", get_size_suffix(type),
            get_reg_alias(&reg_rax, type));
  } else if (is_floating_point(type)) {
    fprintf(codegen_output, "  movs%c %%xmm0, (%%rdi)\n",
            get_floating_point_suffix(type));
  } else if (type->kind == STRUCT || type->kind == UNION) {
    fprintf(codegen_output, "  movq %%rax, %%rsi\n");
    fprintf(codegen_output, "  movq $%d, %%rcx\n", type_size(type));
    fprintf(codegen_output, "  rep movsb\n");
  } else
    error("invalid type");
}
