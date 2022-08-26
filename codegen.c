
#include <stdio.h>

#include "codegen.h"
#include "error.h"
#include "parse.h"
#include "type.h"

#ifndef __STDC__

int printf();
int fprintf();

#endif

static void codegen_str_literal(FILE *codegen_output, StrLiteral *sl);
static void codegen_var_definition(FILE *codegen_output, Tree *var);
static void codegen_function(FILE *codegen_output, Tree *func);
static void codegen_stmt(FILE *codegen_output, Tree *stmt);
static void codegen_addr(FILE *codegen_output, Tree *stmt);

static void size_extend_rax(FILE *codegen_output, Type *a);

static void load2rax_from_raxaddr(FILE *codegen_output, Type *type);
static void store2rdiaddr_from_rax(FILE *codegen_output, Type *type);

extern const char *call_register64[6];
extern const char *call_register32[6];
extern const char *call_register8[6];

extern const char reg_rax[4];

Obj *current_function;

void codegen_translation_unit(FILE *codegen_output, Tree *head) {

  fprintf(codegen_output, ".intel_syntax noprefix\n");

  for (StrLiteral *now = str_literals; now; now = now->next) {
    codegen_str_literal(codegen_output, now);
  }

  Tree *cur = head;
  while (cur) {
    if (cur->kind == DECLARATION && cur->declarator &&
        !cur->decl_specs->has_typedef && cur->def_obj->is_defined) {
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

  fprintf(codegen_output, "  .section	.note.GNU-stack,\"\",@progbits\n");
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
  Obj *obj = var->def_obj;
  fprintf(codegen_output, "  .globl %.*s\n", obj->obj_len, obj->obj_name);

  if (var->declarator->init_expr)
    fprintf(codegen_output, "  .data\n");
  else
    fprintf(codegen_output, "  .bss\n");

  fprintf(codegen_output, "  .align %d\n", type_alignment(obj->type));
  fprintf(codegen_output, "%.*s:\n", obj->obj_len, obj->obj_name);

  if (var->declarator->init_expr) {
    if (type_size(obj->type) == 1) {
      fprintf(codegen_output, "  .byte %d\n",
              eval_constexpr(var->declarator->init_expr));
    } else if (type_size(obj->type) == 4) {
      fprintf(codegen_output, "  .long %d\n",
              eval_constexpr(var->declarator->init_expr));
    } else if (type_size(obj->type) == 8) {
      fprintf(codegen_output, "  .quad %d\n",
              eval_constexpr(var->declarator->init_expr));
    } else
      not_implemented(__func__);
  } else
    fprintf(codegen_output, "  .zero %d\n", type_size(obj->type));
}

void codegen_function(FILE *codegen_output, Tree *func) {

  current_function = func->def_obj;

  fprintf(codegen_output, "  .text\n");
  if (func->decl_specs->has_static)
    fprintf(codegen_output, ".local %.*s\n", func->def_obj->obj_len,
            func->def_obj->obj_name);
  else
    fprintf(codegen_output, ".globl %.*s\n", func->def_obj->obj_len,
            func->def_obj->obj_name);
  fprintf(codegen_output, "%.*s:\n", func->def_obj->obj_len,
          func->def_obj->obj_name);

  fprintf(codegen_output, "  push rbp\n");
  fprintf(codegen_output, "  mov rbp, rsp\n");
  fprintf(codegen_output, "  sub rsp, %d\n", func->def_obj->stack_size);

  if (func->declarator->has_variable_arg) {
    for (int i = 5; i >= 0; i--) {
      fprintf(codegen_output, "  push %s\n", call_register64[i]);
    }
  }

  Tree *cur = getargs_declarator(func->declarator);
  int count = 0;
  while (cur) {
    if (type_size(cur->def_obj->type) == 8)
      fprintf(codegen_output, "  mov [rbp - %d], %s\n",
              cur->def_obj->rbp_offset, call_register64[count]);
    else if (type_size(cur->def_obj->type) == 4)
      fprintf(codegen_output, "  mov [rbp - %d], %s\n",
              cur->def_obj->rbp_offset, call_register32[count]);
    else if (type_size(cur->def_obj->type) == 1)
      fprintf(codegen_output, "  mov [rbp - %d], %s\n",
              cur->def_obj->rbp_offset, call_register8[count]);
    else
      not_implemented(__func__);
    count++;
    cur = cur->next;
  }

  codegen_stmt(codegen_output, func->func_body);

  fprintf(codegen_output, "  mov rsp, rbp\n");
  fprintf(codegen_output, "  pop rbp\n");
  fprintf(codegen_output, "  ret\n");
}

void codegen_addr(FILE *codegen_output, Tree *stmt) {
  if (stmt->kind == VAR) {
    if (!stmt->var_obj->is_global)
      fprintf(codegen_output, "  lea rax, [rbp - %d]\n",
              stmt->var_obj->rbp_offset);
    else if (stmt->var_obj->is_defined)
      fprintf(codegen_output, "  lea rax, [rip + %.*s]\n",
              stmt->var_obj->obj_len, stmt->var_obj->obj_name);
    else
      fprintf(codegen_output, "  mov rax, [%.*s@GOTPCREL + rip]\n",
              stmt->var_obj->obj_len, stmt->var_obj->obj_name);
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
  Tree *cur;
  switch (stmt->kind) {
  case DECLARATION:
    if (stmt->declarator && stmt->declarator->init_expr) {
      fprintf(codegen_output, "  nop\n");
      fprintf(codegen_output, "  lea rdi, [rbp - %d]\n",
              stmt->def_obj->rbp_offset);
      fprintf(codegen_output, "  push rdi\n");
      codegen_stmt(codegen_output, stmt->declarator->init_expr);
      if (stmt->def_obj->type->kind == STRUCT ||
          stmt->def_obj->type->kind == UNION) {
        fprintf(codegen_output, "  pop rdi\n");
        fprintf(codegen_output, "  mov rsi, rax\n");
        fprintf(codegen_output, "  mov rcx, %d\n",
                type_size(stmt->def_obj->type));
        fprintf(codegen_output, "  rep movsb\n");
      } else {
        fprintf(codegen_output, "  pop rdi\n");
        store2rdiaddr_from_rax(codegen_output, stmt->def_obj->type);
      }
    }
    return;
  case LABEL:
    fprintf(codegen_output, ".Llabel%.*s:\n", stmt->label_len,
            stmt->label_name);
    codegen_stmt(codegen_output, stmt->lhs);
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
  case COMPOUND_STMT:
    cur = stmt->stmts;
    while (cur) {
      codegen_stmt(codegen_output, cur);
      cur = cur->next;
    }
    return;
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
  case ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    if (stmt->lhs->type->kind == STRUCT || stmt->lhs->type->kind == UNION) {
      fprintf(codegen_output, "  pop rdi\n");
      fprintf(codegen_output, "  mov rsi, rax\n");
      fprintf(codegen_output, "  mov rcx, %d\n", type_size(stmt->lhs->type));
      fprintf(codegen_output, "  rep movsb\n");
    } else if (stmt->lhs->type->kind == BOOL) {
      fprintf(codegen_output, "  cmp rax,0\n");
      fprintf(codegen_output, "  setne al\n");
      fprintf(codegen_output, "  pop rdi\n");
      fprintf(codegen_output, "  mov [rdi],al\n");
    } else if (is_integer(stmt->lhs->type)) {
      fprintf(codegen_output, "  pop rdi\n");
      store2rdiaddr_from_rax(codegen_output, stmt->type);
      size_extend_rax(codegen_output, stmt->lhs->type);
    } else {
      fprintf(codegen_output, "  pop rdi\n");
      store2rdiaddr_from_rax(codegen_output, stmt->type);
    }
    return;
  case ADD_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  mov rax,rsi\n");
    load2rax_from_raxaddr(codegen_output, stmt->lhs->type);
    // lhs:rax, rhs:rdi
    if (stmt->lhs->type->kind == PTR || stmt->lhs->type->kind == ARRAY)
      fprintf(codegen_output, "  imul rdi,%d\n",
              type_size(stmt->lhs->type->ptr_to));
    else if (stmt->rhs->type->kind == PTR || stmt->rhs->type->kind == ARRAY)
      fprintf(codegen_output, "  imul rax,%d\n",
              type_size(stmt->rhs->type->ptr_to));
    fprintf(codegen_output, "  add rax,rdi\n");

    // store
    fprintf(codegen_output, "  mov rdi,rsi\n");
    store2rdiaddr_from_rax(codegen_output, stmt->lhs->type);
    return;
  case SUB_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  mov rax,rsi\n");
    load2rax_from_raxaddr(codegen_output, stmt->lhs->type);
    // sub
    if (stmt->lhs->type->kind == PTR && is_integer(stmt->rhs->type))
      fprintf(codegen_output, "  imul rdi, %d\n",
              type_size(stmt->lhs->type->ptr_to));
    fprintf(codegen_output, "  sub rax, rdi\n");

    // store
    fprintf(codegen_output, "  mov rdi,rsi\n");
    store2rdiaddr_from_rax(codegen_output, stmt->lhs->type);
    return;
  case MUL_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  imul rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case DIV_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  cqo\n");
    fprintf(codegen_output, "  idiv rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case MOD_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  cqo\n");
    fprintf(codegen_output, "  idiv rdi\n");
    fprintf(codegen_output, "  mov rax,rdx\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case AND_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  and rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case OR_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  or rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case XOR_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  xor rax, rdi\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case LSHIFT_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  mov rcx, rdi\n");
    fprintf(codegen_output, "  sal rax, cl\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case RSHIFT_ASSIGN:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  push rax\n");
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    fprintf(codegen_output, "  pop rsi\n");
    fprintf(codegen_output, "  movsxd rax,[rsi]\n");
    fprintf(codegen_output, "  mov rcx, rdi\n");
    fprintf(codegen_output, "  sar rax, cl\n");
    fprintf(codegen_output, "  mov [rsi], eax\n");
    return;
  case CONDITIONAL:
    codegen_stmt(codegen_output, stmt->cond);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Ltrue%d\n", stmt->label_number);
    fprintf(codegen_output, "  jmp .Lfalse%d\n", stmt->label_number);
    fprintf(codegen_output, ".Ltrue%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lfalse%d:\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case COMMA:
    codegen_stmt(codegen_output, stmt->lhs);
    codegen_stmt(codegen_output, stmt->rhs);
    return;
  case LOGICAL_OR:
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Ltrue%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  jne .Ltrue%d\n", stmt->label_number);
    fprintf(codegen_output, "  mov rax, 0\n");
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    fprintf(codegen_output, ".Ltrue%d:\n", stmt->label_number);
    fprintf(codegen_output, "  mov rax, 1\n");
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case LOGICAL_AND:
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lfalse%d\n", stmt->label_number);
    codegen_stmt(codegen_output, stmt->rhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  je .Lfalse%d\n", stmt->label_number);
    fprintf(codegen_output, "  mov rax, 1\n");
    fprintf(codegen_output, "  jmp .Lend%d\n", stmt->label_number);
    fprintf(codegen_output, ".Lfalse%d:\n", stmt->label_number);
    fprintf(codegen_output, "  mov rax, 0\n");
    fprintf(codegen_output, ".Lend%d:\n", stmt->label_number);
    return;
  case LOGICAL_NOT:
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  cmp rax,0\n");
    fprintf(codegen_output, "  sete al\n");
    fprintf(codegen_output, "  movsx rax,al\n");
    return;
  case BIT_NOT:
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  not rax\n");
    return;
  case CAST:
    codegen_stmt(codegen_output, stmt->lhs);
    return;
  case PLUS:
    codegen_stmt(codegen_output, stmt->lhs);
    return;
  case MINUS:
    codegen_stmt(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  neg rax\n");
    return;
  case ADDR:
    codegen_addr(codegen_output, stmt->lhs);
    return;
  case DEREF:
    codegen_stmt(codegen_output, stmt->lhs);
    if (stmt->type->kind == ARRAY || stmt->type->kind == STRUCT ||
        stmt->type->kind == UNION || stmt->type->kind == FUNC)
      return;
    load2rax_from_raxaddr(codegen_output, stmt->type);
    return;
  case FUNC_CALL: {
    fprintf(codegen_output, "  mov r10,rsp\n");
    fprintf(codegen_output, "  and rsp,0xfffffffffffffff0\n");
    fprintf(codegen_output, "  push r10\n");
    fprintf(codegen_output, "  push 0\n");
    int stack_count = 0;
    Tree *cur = stmt->call_args;
    while (cur) {
      codegen_stmt(codegen_output, cur);
      fprintf(codegen_output, "  push rax\n");
      stack_count++;
      cur = cur->next;
    }
    if (stack_count > 6)
      error("more than 6 arguments are not implemented");
    codegen_stmt(codegen_output, stmt->lhs);
    for (int i = 0; i < stack_count; i++)
      fprintf(codegen_output, "  pop %s\n", call_register64[i]);
    fprintf(codegen_output, "  call rax\n");
    fprintf(codegen_output, "  pop r10\n");
    fprintf(codegen_output, "  pop rsp\n");
  }
    return;
  case POST_INCREMENT:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    load2rax_from_raxaddr(codegen_output, stmt->lhs->type);
    fprintf(codegen_output, "  mov rsi,rax\n");
    if (stmt->lhs->type->kind == PTR)
      fprintf(codegen_output, "  mov rdx, %d\n",
              type_size(stmt->lhs->type->ptr_to));
    else
      fprintf(codegen_output, "  mov rdx, 1\n");
    fprintf(codegen_output, "  add rsi, rdx\n");
    fprintf(codegen_output, "  mov [rdi],esi\n");
    return;
  case POST_DECREMENT:
    codegen_addr(codegen_output, stmt->lhs);
    fprintf(codegen_output, "  mov rdi,rax\n");
    load2rax_from_raxaddr(codegen_output, stmt->lhs->type);
    fprintf(codegen_output, "  mov rsi,rax\n");
    if (stmt->lhs->type->kind == PTR)
      fprintf(codegen_output, "  mov rdx, %d\n",
              type_size(stmt->lhs->type->ptr_to));
    else
      fprintf(codegen_output, "  mov rdx, 1\n");
    fprintf(codegen_output, "  sub rsi, rdx\n");
    fprintf(codegen_output, "  mov [rdi],esi\n");
    return;
  case DOT:
    codegen_addr(codegen_output, stmt);
    if (stmt->type->kind == ARRAY || stmt->type->kind == STRUCT ||
        stmt->type->kind == UNION)
      return;
    load2rax_from_raxaddr(codegen_output, stmt->type);
    return;
  case ARROW:
    codegen_addr(codegen_output, stmt);
    if (stmt->type->kind == ARRAY || stmt->type->kind == STRUCT ||
        stmt->type->kind == UNION)
      return;
    load2rax_from_raxaddr(codegen_output, stmt->type);
    return;
  case NUM:
    fprintf(codegen_output, "  mov rax, %d\n", stmt->num);
    return;
  case STR:
    fprintf(codegen_output, "  lea rax, [rip + .LC%d]\n",
            stmt->str_literal->id);
    return;
  case VAR:
    codegen_addr(codegen_output, stmt);
    if (stmt->type->kind == FUNC || stmt->type->kind == ARRAY ||
        stmt->type->kind == STRUCT || stmt->type->kind == UNION)
      return;
    load2rax_from_raxaddr(codegen_output, stmt->type);
    return;
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
  default:
    break;
  }

  // arithmetic-op

  codegen_stmt(codegen_output, stmt->lhs);
  fprintf(codegen_output, "  push rax\n");
  codegen_stmt(codegen_output, stmt->rhs);
  fprintf(codegen_output, "  mov rdi,rax\n");
  fprintf(codegen_output, "  pop rax\n");

  switch (stmt->kind) {
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
    if (stmt->lhs->type->kind == PTR || stmt->lhs->type->kind == ARRAY)
      fprintf(codegen_output, "  imul rdi,%d\n",
              type_size(stmt->lhs->type->ptr_to));
    else if (stmt->rhs->type->kind == PTR || stmt->rhs->type->kind == ARRAY)
      fprintf(codegen_output, "  imul rax,%d\n",
              type_size(stmt->rhs->type->ptr_to));
    fprintf(codegen_output, "  add rax,rdi\n");
    break;
  case SUB:
    if (stmt->lhs->type->kind == PTR && is_integer(stmt->rhs->type))
      fprintf(codegen_output, "  imul rdi, %d\n",
              type_size(stmt->lhs->type->ptr_to));
    fprintf(codegen_output, "  sub rax,rdi\n");
    if (stmt->lhs->type->kind == PTR && stmt->rhs->type->kind == PTR) {
      fprintf(codegen_output, "  mov rdi, %d\n",
              type_size(stmt->lhs->type->ptr_to));
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
    fprintf(codegen_output, "  movsxd rax,[rax]\n");
  else if (type_size(type) == 1)
    fprintf(codegen_output, "movsx rax, BYTE PTR [rax]\n");
  else
    not_implemented(__func__);
}

void store2rdiaddr_from_rax(FILE *codegen_output, Type *type) {
  if (type_size(type) == 8)
    fprintf(codegen_output, "  mov [rdi],rax\n");
  else if (type_size(type) == 4)
    fprintf(codegen_output, "  mov [rdi],eax\n");
  else if (type_size(type) == 1)
    fprintf(codegen_output, "  mov [rdi],al\n");
  else
    not_implemented(__func__);
}
