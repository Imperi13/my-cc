
#include <stdio.h>
#include <string.h>

#include "codegen.h"
#include "constexpr.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void codegen_str_literal(FILE *codegen_output, StrLiteral *sl);
static void codegen_var_definition(FILE *codegen_output, Tree *var);
static void codegen_function(FILE *codegen_output, Tree *func);
static void codegen_stmt(FILE *codegen_output, Tree *stmt);
static void codegen_expr(FILE *codegen_output, Tree *expr);
static void codegen_addr(FILE *codegen_output, Tree *stmt);

static void store2rdiaddr_local_var_initialize(FILE *codegen_output,
                                               Type *var_type, Tree *init_val);

static void size_extend_rax(FILE *codegen_output, Type *a);

static void load2rax_from_raxaddr(FILE *codegen_output, Type *type);
static void store2rdiaddr_from_rax(FILE *codegen_output, Type *type);

const char *call_register64[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
const char *call_register32[6] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
const char *call_register8[6] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};

Obj *current_function = NULL;

void codegen_translation_unit(FILE *codegen_output, Tree *head) {

  fprintf(codegen_output, ".intel_syntax noprefix\n");

  for (StrLiteral *now = str_literals; now; now = now->next) {
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

  fprintf(codegen_output, "  push rbp\n");
  fprintf(codegen_output, "  mov rbp, rsp\n");
  fprintf(codegen_output, "  sub rsp, %d\n", current_function->stack_size);

  if (func->declarator->has_variable_arg) {
    for (int i = 5; i >= 0; i--) {
      fprintf(codegen_output, "  push %s\n", call_register64[i]);
    }
  }

  Tree *cur = getargs_declarator(func->declarator);
  int count = 0;
  while (cur) {
    Obj *cur_obj = cur->declarator->def_obj;
    if (count < 6) {
      if (type_size(cur_obj->type) == 8)
        fprintf(codegen_output, "  mov [rbp - %d], %s\n", cur_obj->rbp_offset,
                call_register64[count]);
      else if (type_size(cur_obj->type) == 4)
        fprintf(codegen_output, "  mov [rbp - %d], %s\n", cur_obj->rbp_offset,
                call_register32[count]);
      else if (type_size(cur_obj->type) == 1)
        fprintf(codegen_output, "  mov [rbp - %d], %s\n", cur_obj->rbp_offset,
                call_register8[count]);
      else
        not_implemented(__func__);
    } else {
      if (type_size(cur_obj->type) == 8) {
        fprintf(codegen_output, "  mov rax, [rbp + %d]\n",
                0x10 + 0x8 * (count - 6));
        fprintf(codegen_output, "  mov [rbp - %d], rax\n", cur_obj->rbp_offset);
      } else if (type_size(cur_obj->type) == 4) {
        fprintf(codegen_output, "  mov eax, [rbp + %d]\n",
                0x10 + 0x8 * (count - 6));
        fprintf(codegen_output, "  mov [rbp - %d], eax\n", cur_obj->rbp_offset);
      } else if (type_size(cur_obj->type) == 1) {
        fprintf(codegen_output, "  mov al, [rbp + %d]\n",
                0x10 + 0x8 * (count - 6));
        fprintf(codegen_output, "  mov [rbp - %d], al\n", cur_obj->rbp_offset);
      } else
        not_implemented(__func__);
    }
    count++;
    cur = cur->next;
  }

  codegen_stmt(codegen_output, func->func_body);

  fprintf(codegen_output, "  mov rsp, rbp\n");
  fprintf(codegen_output, "  pop rbp\n");
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
    fprintf(codegen_output, "  push rdi\n");
    codegen_stmt(codegen_output, init_val);
    fprintf(codegen_output, "  pop rdi\n");
    store2rdiaddr_from_rax(codegen_output, var_type);
  } else if (var_type->kind == ARRAY) {
    int cnt = 0;
    fprintf(codegen_output, "  push rdi\n");

    for (InitializeList *cur = init_val->init_list; cur; cur = cur->next) {
      fprintf(codegen_output, "  pop rdi\n");
      fprintf(codegen_output, "  push rdi\n");
      fprintf(codegen_output, "  add rdi, %d\n",
              cnt * type_size(var_type->ptr_to));
      store2rdiaddr_local_var_initialize(codegen_output, var_type->ptr_to,
                                         cur->init_val);
      cnt++;
    }

    fprintf(codegen_output, "  pop rdi\n");

  } else if (var_type->kind == STRUCT) {
    fprintf(codegen_output, "  push rdi\n");

    Member *mem_cur = var_type->st_def->members;

    for (InitializeList *cur = init_val->init_list; cur; cur = cur->next) {
      if (cur->member_name) {
        while (strcmp(cur->member_name, mem_cur->member_name) != 0)
          mem_cur = mem_cur->next;
      }
      fprintf(codegen_output, "  pop rdi\n");
      fprintf(codegen_output, "  push rdi\n");
      fprintf(codegen_output, "  add rdi, %d\n", mem_cur->offset);
      store2rdiaddr_local_var_initialize(codegen_output, mem_cur->type,
                                         cur->init_val);
      mem_cur = mem_cur->next;
    }

    fprintf(codegen_output, "  pop rdi\n");

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
      fprintf(codegen_output, "  lea rax, [rbp - %d]\n",
              stmt->var_obj->rbp_offset);
    else if (stmt->var_obj->is_defined)
      fprintf(codegen_output, "  lea rax, [rip + %s]\n",
              stmt->var_obj->obj_name);
    else
      fprintf(codegen_output, "  mov rax, [%s@GOTPCREL + rip]\n",
              stmt->var_obj->obj_name);
  } else if (stmt->kind == DEREF) {
    codegen_stmt(codegen_output, stmt->lhs);
  } else if (stmt->kind == DOT) {
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  add rax, %d\n", stmt->member->offset);
  } else if (stmt->kind == ARROW) {
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  add rax, %d\n", stmt->member->offset);
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
        fprintf(codegen_output, "  lea rdi, [rbp - %d]\n",
                cur->def_obj->rbp_offset);
        store2rdiaddr_local_var_initialize(codegen_output, cur->def_obj->type,
                                           cur->init_expr);
      }
    }
  }
    return;
  case LABEL:
    fprintf(codegen_output, ".Llabel.%s:\n", stmt->label_name);
    codegen_stmt(codegen_output, stmt->lhs);
    return;
  case GOTO:
    fprintf(codegen_output, "  jmp .Llabel.%s\n", stmt->label_name);
    return;
  case CASE:
    fprintf(codegen_output, ".Lswitch%d_case%d:\n", stmt->label_number,
            stmt->case_num);
    codegen_stmt(codegen_output, stmt->lhs);
    return;
  case DEFAULT:
    fprintf(codegen_output, ".Lswitch%d_default:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    return;
  case RETURN:
    if (stmt->lhs)
      codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  mov rsp, rbp\n");
    fprintf(codegen_output, "  pop rbp\n");
    fprintf(codegen_output, "  ret\n");
    return;
  case BREAK:
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    return;
  case CONTINUE:
    fprintf(codegen_output, "  jmp .Lloopend%d\n", stmt->label_number);
    return;
  case COMPOUND_STMT: {
    Tree *cur = stmt->stmts;
    while (cur) {
      codegen_stmt(codegen_output, cur);
      cur = cur->next;
    }
    return;
  }
  case WHILE:
    fprintf(codegen_output, ".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lend%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lloopend%d:\n", stmt->label_number);
    fprintf(codegen_output, "  jmp .Lbegin%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case DO_WHILE:
    fprintf(codegen_output, ".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lloopend%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Lbegin%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case FOR:
    if (stmt->for_init)
      codegen_stmt(codegen_output, stmt->for_init);
    fprintf(codegen_output, ".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lend%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lloopend%d:\n", stmt->label_number);
    if (stmt->for_update)
      codegen_stmt(codegen_output, stmt->for_update);
    fprintf(codegen_output, "  jmp .Lbegin%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case IF:
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lelse%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lelse%d:\n", stmt->label_number);
    if (stmt->rhs)
      codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case SWITCH:
    codegen_stmt(codegen_output, stmt->cond);
    for (Case *cur = stmt->cases; cur; cur = cur->next) {
      fprintf(codegen_output, "  cmp rax, %d\n", cur->case_num);
      fprintf(codegen_output, "  je .Lswitch%d_case%d\n", stmt->label_number,
              cur->case_num);
    }
    if (stmt->has_default)
      fprintf(codegen_output, "  jmp .Lswitch%d_default\n", stmt->label_number);
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;

    // codegen builtin_function here
  case BUILTIN_VA_START: {
    Obj *va_obj = stmt->lhs->var_obj;
    Obj *last_para = stmt->rhs->var_obj;

    fprintf(codegen_output, "  mov eax,%d\n", last_para->nth_arg * 0x8);
    fprintf(codegen_output, "  mov [rbp - %d], eax\n", va_obj->rbp_offset);
    fprintf(codegen_output, "  mov eax,0x30\n");
    fprintf(codegen_output, "  mov [rbp - %d], eax\n",
            va_obj->rbp_offset - 0x4);
    fprintf(codegen_output, "  lea rax,[rbp + 0x10]\n");
    fprintf(codegen_output, "  mov [rbp - %d], rax\n",
            va_obj->rbp_offset - 0x8);
    fprintf(codegen_output, "  lea rax,[rbp - %d]\n",
            current_function->stack_size + 0x30);
    fprintf(codegen_output, "  mov [rbp - %d], rax\n",
            va_obj->rbp_offset - 0x10);
  }
    return;
  case BUILTIN_VA_END: {
    Obj *va_obj = stmt->lhs->var_obj;
    fprintf(codegen_output, "  mov rax, 0x0\n");
    fprintf(codegen_output, "  mov [rbp - %d],rax\n", va_obj->rbp_offset);
    fprintf(codegen_output, "  mov [rbp - %d],rax\n", va_obj->rbp_offset - 0x8);
    fprintf(codegen_output, "  mov [rbp - %d],rax\n",
            va_obj->rbp_offset - 0x10);
  }
    return;

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
    return;
  }

  default:
    error("invalid ast kind");
  }
}

void codegen_expr(FILE *codegen_output, Tree *expr) {
  assert(expr, "expr is NULL");

  switch (expr->kind) {
  case ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  pop rdi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);

    if (is_integer(expr->lhs->type))
      size_extend_rax(codegen_output, expr->lhs->type);

    return;
  case ADD_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  mov rax,rsi\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);
    // lhs:rax, rhs:rdi
    if (expr->lhs->type->kind == PTR)
      fprintf(codegen_output, "  imul rdi,%d\n",
              type_size(expr->lhs->type->ptr_to));
    else if (expr->rhs->type->kind == PTR)
      fprintf(codegen_output, "  imul rax,%d\n",
              type_size(expr->rhs->type->ptr_to));
    fprintf(codegen_output, "  add rax,rdi\n");

    // store
    fprintf(codegen_output, "  mov rdi,rsi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    return;
  case SUB_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  mov rax,rsi\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);
    // sub
    if (expr->lhs->type->kind == PTR && is_integer(expr->rhs->type))
      fprintf(codegen_output, "  imul rdi, %d\n",
              type_size(expr->lhs->type->ptr_to));
    fprintf(codegen_output, "  sub rax, rdi\n");

    // store
    fprintf(codegen_output, "  mov rdi,rsi\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    return;
  case MUL_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  imul rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case DIV_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  cqo\n");
    fprintf(codegen_output, "  idiv rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case MOD_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  cqo\n");
    fprintf(codegen_output, "  idiv rdi\n");
    fprintf(codegen_output, "  mov rax,rdx\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case AND_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  and rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case OR_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  or rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case XOR_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  xor rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case LSHIFT_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  mov rcx, rdi\n");
    fprintf(codegen_output, "  sal rax, cl\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case RSHIFT_ASSIGN:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  mov rcx, rdi\n");
    fprintf(codegen_output, "  sar rax, cl\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case CONDITIONAL:
    codegen_stmt(codegen_output, expr->cond);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Ltrue%d\n", expr->label_number);
    fprintf(codegen_output, "  jmp .Lfalse%d\n", expr->label_number);
    fprintf(codegen_output, ".Ltrue%d:\n", expr->label_number);
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  jmp .Lend%d\n", expr->label_number);
    fprintf(codegen_output, ".Lfalse%d:\n", expr->label_number);
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, ".Lend%d:\n", expr->label_number);
    return;
  case COMMA:
    codegen_stmt(codegen_output, expr->lhs);
    codegen_stmt(codegen_output, expr->rhs);
    return;
  case LOGICAL_OR:
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Ltrue%d\n", expr->label_number);
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Ltrue%d\n", expr->label_number);
    fprintf(codegen_output, "  mov rax, 0\n");
    fprintf(codegen_output, "  jmp .Lend%d\n", expr->label_number);
    fprintf(codegen_output, ".Ltrue%d:\n", expr->label_number);
    fprintf(codegen_output, "  mov rax, 1\n");
    fprintf(codegen_output, ".Lend%d:\n", expr->label_number);
    return;
  case LOGICAL_AND:
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lfalse%d\n", expr->label_number);
    codegen_stmt(codegen_output, expr->rhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lfalse%d\n", expr->label_number);
    fprintf(codegen_output, "  mov rax, 1\n");
    fprintf(codegen_output, "  jmp .Lend%d\n", expr->label_number);
    fprintf(codegen_output, ".Lfalse%d:\n", expr->label_number);
    fprintf(codegen_output, "  mov rax, 0\n");
    fprintf(codegen_output, ".Lend%d:\n", expr->label_number);
    return;
  case LOGICAL_NOT:
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  sete al\n");
    fprintf(codegen_output, "  movsx rax,al\n");
    return;
  case BIT_NOT:
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  not rax\n");
    return;
  case CAST:
    if (expr->type->kind == BOOL) {
      codegen_stmt(codegen_output, expr->lhs);
      fprintf(codegen_output, "  cmp rax,0\n");
      fprintf(codegen_output, "  setne al\n");
    } else {
      codegen_stmt(codegen_output, expr->lhs);
    }
    return;
  case PLUS:
    codegen_stmt(codegen_output, expr->lhs);
    return;
  case MINUS:
    codegen_stmt(codegen_output, expr->lhs);
    fprintf(codegen_output, "  neg rax\n");
    return;
  case ADDR:
    codegen_addr(codegen_output, expr->lhs);
    return;
  case DEREF:
    codegen_stmt(codegen_output, expr->lhs);
    if (expr->type->kind == ARRAY || expr->type->kind == STRUCT ||
        expr->type->kind == UNION || expr->type->kind == FUNC)
      return;
    load2rax_from_raxaddr(codegen_output, expr->type);
    return;
  case FUNC_CALL: {
    fprintf(codegen_output, "  mov r10,rsp\n");
    fprintf(codegen_output, "  and rsp,0xfffffffffffffff0\n");
    fprintf(codegen_output, "  push r10\n");
    fprintf(codegen_output, "  push 0\n");

    long call_arg_size = size_vector(expr->call_args_vector);

    bool need_padding = (call_arg_size > 6) && (call_arg_size % 2 == 1);
    if (need_padding)
      fprintf(codegen_output, "  push 0\n");

    for (long i = call_arg_size - 1; i >= 0; i--) {
      Tree *arg = get_vector(expr->call_args_vector, i);
      codegen_stmt(codegen_output, arg);
      fprintf(codegen_output, "  push rax\n");
    }

    codegen_stmt(codegen_output, expr->lhs);
    for (int i = 0; i < ((call_arg_size > 6) ? 6 : call_arg_size); i++)
      fprintf(codegen_output, "  pop %s\n", call_register64[i]);
    fprintf(codegen_output, "  call rax\n");

    // clean stack_arg
    for (int i = 0; i < ((call_arg_size > 6) ? call_arg_size - 6 : 0); i++)
      fprintf(codegen_output, " pop r10\n");

    if (need_padding)
      fprintf(codegen_output, "  pop r10\n");

    fprintf(codegen_output, "  pop r10\n");
    fprintf(codegen_output, "  pop rsp\n");
  }
    return;
  case POST_INCREMENT:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);
    fprintf(codegen_output, "  push rax\n");
    if (expr->lhs->type->kind == PTR)
      fprintf(codegen_output, "  mov rdx, %d\n",
              type_size(expr->lhs->type->ptr_to));
    else
      fprintf(codegen_output, "  mov rdx, 1\n");
    fprintf(codegen_output, "  add rax, rdx\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    fprintf(codegen_output, "  pop rax\n");
    return;
  case POST_DECREMENT:
    codegen_addr(codegen_output, expr->lhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    load2rax_from_raxaddr(codegen_output, expr->lhs->type);
    fprintf(codegen_output, "  push rax\n");
    if (expr->lhs->type->kind == PTR)
      fprintf(codegen_output, "  mov rdx, %d\n",
              type_size(expr->lhs->type->ptr_to));
    else
      fprintf(codegen_output, "  mov rdx, 1\n");
    fprintf(codegen_output, "  sub rax, rdx\n");
    store2rdiaddr_from_rax(codegen_output, expr->lhs->type);
    fprintf(codegen_output, "  pop rax\n");
    return;
  case DOT:
    codegen_addr(codegen_output, expr);
    if (expr->type->kind == ARRAY || expr->type->kind == STRUCT ||
        expr->type->kind == UNION)
      return;
    load2rax_from_raxaddr(codegen_output, expr->type);
    return;
  case ARROW:
    codegen_addr(codegen_output, expr);
    if (expr->type->kind == ARRAY || expr->type->kind == STRUCT ||
        expr->type->kind == UNION)
      return;
    load2rax_from_raxaddr(codegen_output, expr->type);
    return;
  case NUM:
    fprintf(codegen_output, "  mov rax, %d\n", expr->num);
    return;
  case STR:
    fprintf(codegen_output, "  lea rax, [rip + .LC%d]\n",
            expr->str_literal->id);
    return;
  case VAR:
    codegen_addr(codegen_output, expr);
    if (expr->type->kind == FUNC || expr->type->kind == ARRAY ||
        expr->type->kind == STRUCT || expr->type->kind == UNION)
      return;
    load2rax_from_raxaddr(codegen_output, expr->type);
    return;
  default:
    break;
  }

  // binary_op

  codegen_stmt(codegen_output, expr->lhs);
  fprintf(codegen_output, "  push rax\n");
  codegen_stmt(codegen_output, expr->rhs);
  fprintf(codegen_output, "  mov rdi,rax\n");
  fprintf(codegen_output, "  pop rax\n");

  switch (expr->kind) {
  case BIT_AND:
    fprintf(codegen_output, "  and rax, rdi\n");
    break;
  case BIT_XOR:
    fprintf(codegen_output, "  xor rax, rdi\n");
    break;
  case BIT_OR:
    fprintf(codegen_output, "  or rax, rdi\n");
    break;
  case EQUAL:
    fprintf(codegen_output, "  cmp rax,rdi\n");
    fprintf(codegen_output, "  sete al\n");
    fprintf(codegen_output, "  movzb rax,al\n");
    break;
  case NOT_EQUAL:
    fprintf(codegen_output, "  cmp rax,rdi\n");
    fprintf(codegen_output, "  setne al\n");
    fprintf(codegen_output, "  movzb rax,al\n");
    break;
  case SMALLER:
    fprintf(codegen_output, "  cmp rax,rdi\n");
    fprintf(codegen_output, "  setl al\n");
    fprintf(codegen_output, "  movzb rax,al\n");
    break;
  case SMALLER_EQUAL:
    fprintf(codegen_output, "  cmp rax,rdi\n");
    fprintf(codegen_output, "  setle al\n");
    fprintf(codegen_output, "  movzb rax,al\n");
    break;
  case GREATER:
    fprintf(codegen_output, "  cmp rdi,rax\n");
    fprintf(codegen_output, "  setl al\n");
    fprintf(codegen_output, "  movzb rax,al\n");
    break;
  case GREATER_EQUAL:
    fprintf(codegen_output, "  cmp rdi,rax\n");
    fprintf(codegen_output, "  setle al\n");
    fprintf(codegen_output, "  movzb rax,al\n");
    break;
  case LSHIFT:
    fprintf(codegen_output, "  mov rcx, rdi\n");
    fprintf(codegen_output, "  sal rax, cl\n");
    break;
  case RSHIFT:
    fprintf(codegen_output, "  mov rcx, rdi\n");
    fprintf(codegen_output, "  sar rax, cl\n");
    break;
  case ADD:
    if (expr->lhs->type->kind == PTR)
      fprintf(codegen_output, "  imul rdi,%d\n",
              type_size(expr->lhs->type->ptr_to));
    else if (expr->rhs->type->kind == PTR)
      fprintf(codegen_output, "  imul rax,%d\n",
              type_size(expr->rhs->type->ptr_to));
    fprintf(codegen_output, "  add rax,rdi\n");
    break;
  case SUB:
    if (expr->lhs->type->kind == PTR && is_integer(expr->rhs->type))
      fprintf(codegen_output, "  imul rdi, %d\n",
              type_size(expr->lhs->type->ptr_to));
    fprintf(codegen_output, "  sub rax,rdi\n");
    if (expr->lhs->type->kind == PTR && expr->rhs->type->kind == PTR) {
      fprintf(codegen_output, "  mov rdi, %d\n",
              type_size(expr->lhs->type->ptr_to));
      fprintf(codegen_output, "  cqo\n");
      fprintf(codegen_output, "  idiv rdi\n");
    }
    break;
  case MUL:
    fprintf(codegen_output, "  imul rax,rdi\n");
    break;
  case DIV:
    fprintf(codegen_output, "  cqo\n");
    fprintf(codegen_output, "  idiv rdi\n");
    break;
  case MOD:
    fprintf(codegen_output, "  cqo\n");
    fprintf(codegen_output, "  idiv rdi\n");
    fprintf(codegen_output, "  mov rax,rdx\n");
    break;
  default:
    error("cannnot codegen binary_op");
    break;
  }
}

void size_extend_rax(FILE *codegen_output, Type *a) {
  if (!is_integer(a))
    error("cannot size-extend");

  if (type_size(a) == 8)
    return;
  else if (type_size(a) == 4)
    fprintf(codegen_output, "  movsx rax, eax\n");
  else if (type_size(a) == 2)
    fprintf(codegen_output, "  movsx rax, ax\n");
  else if (type_size(a) == 1)
    fprintf(codegen_output, "  movsx rax, al\n");
  else
    error("invalid size");
}

// raxレジスタで指しているアドレスからtype型の値をraxにロードする
void load2rax_from_raxaddr(FILE *codegen_output, Type *type) {
  if (type_size(type) == 8)
    fprintf(codegen_output, "  mov rax,[rax]\n");
  else if (type_size(type) == 4)
    fprintf(codegen_output, "  movsx rax, DWORD PTR [rax]\n");
  else if (type_size(type) == 2)
    fprintf(codegen_output, "movsx rax, WORD PTR [rax]\n");
  else if (type_size(type) == 1)
    fprintf(codegen_output, "movsx rax, BYTE PTR [rax]\n");
  else
    not_implemented(__func__);
}

// raxレジスタが表すtype型の値をrdiレジスタが指すアドレスにstoreする
void store2rdiaddr_from_rax(FILE *codegen_output, Type *type) {
  if (type->kind == STRUCT || type->kind == UNION) {
    fprintf(codegen_output, "  mov rsi, rax\n");
    fprintf(codegen_output, "  mov rcx, %d\n", type_size(type));
    fprintf(codegen_output, "  rep movsb\n");
  } else if (type->kind == BOOL) {
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  setne al\n");
    fprintf(codegen_output, "  mov [rdi],al\n");
  } else {
    if (type_size(type) == 8)
      fprintf(codegen_output, "  mov [rdi],rax\n");
    else if (type_size(type) == 4)
      fprintf(codegen_output, "  mov [rdi],eax\n");
    else if (type_size(type) == 2)
      fprintf(codegen_output, "  mov [rdi],ax\n");
    else if (type_size(type) == 1)
      fprintf(codegen_output, "  mov [rdi],al\n");
    else
      error("%s", __func__);
  }
}
