
#include <stdio.h>

#include "codegen.h"
#include "error.h"
#include "parse.h"

static void codegen_function(Tree *func);
static void codegen_stmt(Tree *stmt);

void codegen_translation_unit(Tree *head) {

  printf(".intel_syntax noprefix\n");

  Tree *cur = head;
  while (cur) {
    if (cur->kind == DECLARATION) {
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
  printf("  sub rsp, 0\n");

  codegen_stmt(func->func_body);

  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}

void codegen_stmt(Tree *stmt) {
  Tree *cur;
  switch (stmt->kind) {
  case NUM:
    printf("  mov rax, %ld\n", stmt->num);
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
  case COMMA:
    codegen_stmt(stmt->lhs);
    codegen_stmt(stmt->rhs);
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
