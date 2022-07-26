
#include <stdio.h>

#include "codegen.h"
#include "error.h"
#include "parse.h"

static void codegen_function(Tree *func);
static void codegen_stmt(Tree *stmt);
static void codegen_addr(Tree *stmt);

char call_register64[][4] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

void codegen_translation_unit(Tree *head) {

  printf(".intel_syntax noprefix\n");

  Tree *cur = head;
  while (cur) {
    if (cur->kind == DECLARATION && cur->def_obj->is_defined) {
      not_implemented();
    }
    cur = cur->next;
  }

  cur = head;
  while (cur) {
    if (cur->kind == FUNC_DEF) {
      codegen_function(cur);
    }
    cur = cur->next;
  }
}

void codegen_function(Tree *func) {

  printf("  .text\n");
  printf(".globl %.*s\n", func->declarator->len, func->declarator->name);
  printf("%.*s:\n", func->declarator->len, func->declarator->name);

  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", func->def_obj->stack_size);

  codegen_stmt(func->func_body);

  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}

void codegen_addr(Tree *stmt) {
  if (stmt->kind == VAR) {
    if (!stmt->var_obj->is_global)
      printf("  lea rax, [rbp - %d]\n", stmt->var_obj->rbp_offset);
    else if (stmt->var_obj->is_defined)
      printf("  lea rax, [rip + %.*s]\n", stmt->var_obj->obj_len,
             stmt->var_obj->obj_name);
    else
      printf("  mov rax, [%.*s@GOTPCREL + rip]\n", stmt->var_obj->obj_len,
             stmt->var_obj->obj_name);
  } else {
    not_implemented();
  }
}

void codegen_stmt(Tree *stmt) {
  Tree *cur;
  switch (stmt->kind) {
  case DECLARATION:
    return;
  case RETURN:
    codegen_stmt(stmt->lhs);
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
    return;
  case COMPOUND_STMT:
    cur = stmt->stmts;
    while (cur) {
      codegen_stmt(cur);
      cur = cur->next;
    }
    return;
  case ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  pop rdi\n");
    printf("  mov [rdi],eax\n");
    return;
  case CONDITIONAL:
    codegen_stmt(stmt->cond);
    printf("  cmp rax,0\n");
    printf("  jne .Ltrue%d\n", stmt->label_number);
    printf("  jmp .Lfalse%d\n", stmt->label_number);
    printf(".Ltrue%d:\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    printf("  jmp .Lend%d\n", stmt->label_number);
    printf(".Lfalse%d:\n", stmt->label_number);
    codegen_stmt(stmt->rhs);
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case COMMA:
    codegen_stmt(stmt->lhs);
    codegen_stmt(stmt->rhs);
    return;
  case LOGICAL_NOT:
    codegen_stmt(stmt->lhs);
    printf("  cmp rax,0\n");
    printf("  sete al\n");
    printf("  movsx rax,al\n");
    return;
  case BIT_NOT:
    codegen_stmt(stmt->lhs);
    printf("  not rax\n");
    return;
  case PLUS:
    codegen_stmt(stmt->lhs);
    return;
  case MINUS:
    codegen_stmt(stmt->lhs);
    printf("  neg rax\n");
    return;
  case FUNC_CALL: {
    printf("  mov r10,rsp\n");
    printf("  and rsp,0xfffffffffffffff0\n");
    printf("  push r10\n");
    printf("  push 0\n");
    int stack_count = 0;
    Tree *cur = stmt->args;
    while (cur) {
      codegen_stmt(cur);
      printf("  push rax\n");
      stack_count++;
      cur = cur->next;
    }
    if (stack_count > 6)
      error("more than 6 arguments are not implemented");
    codegen_stmt(stmt->lhs);
    for (int i = 0; i < stack_count; i++)
      printf("  pop %s\n", call_register64[i]);
    printf("  call rax\n");
    printf("  pop r10\n");
    printf("  pop rsp\n");
  }
    return;
  case NUM:
    printf("  mov rax, %ld\n", stmt->num);
    return;
  case VAR:
    codegen_addr(stmt);
    if (stmt->type->kind == FUNC)
      return;
    printf("  mov eax,[rax]\n");
    return;
  default:
    break;
  }

  // arithmetic-op

  codegen_stmt(stmt->lhs);
  printf("  push rax\n");
  codegen_stmt(stmt->rhs);
  printf("  mov rdi,rax\n");
  printf("  pop rax\n");

  switch (stmt->kind) {
  case BIT_AND:
    printf("  and rax, rdi\n");
    break;
  case BIT_XOR:
    printf("  xor rax, rdi\n");
    break;
  case BIT_OR:
    printf("  or rax, rdi\n");
    break;
  case EQUAL:
    printf("  cmp rax,rdi\n");
    printf("  sete al\n");
    printf("  movzb rax,al\n");
    break;
  case NOT_EQUAL:
    printf("  cmp rax,rdi\n");
    printf("  setne al\n");
    printf("  movzb rax,al\n");
    break;
  case SMALLER:
    printf("  cmp rax,rdi\n");
    printf("  setl al\n");
    printf("  movzb rax,al\n");
    break;
  case SMALLER_EQUAL:
    printf("  cmp rax,rdi\n");
    printf("  setle al\n");
    printf("  movzb rax,al\n");
    break;
  case GREATER:
    printf("  cmp rdi,rax\n");
    printf("  setl al\n");
    printf("  movzb rax,al\n");
    break;
  case GREATER_EQUAL:
    printf("  cmp rdi,rax\n");
    printf("  setle al\n");
    printf("  movzb rax,al\n");
    break;
  case LSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  sal rax, cl\n");
    break;
  case RSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  sar rax, cl\n");
    break;
  case ADD:
    printf("  add rax,rdi\n");
    break;
  case SUB:
    printf("  sub rax,rdi\n");
    break;
  case MUL:
    printf("  imul rax,rdi\n");
    break;
  case DIV:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    break;
  case MOD:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    printf("  mov rax,rdx\n");
    break;
  default:
    error("cannnot codegen binary_op");
    break;
  }
}
