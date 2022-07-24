
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
    break;
  case RETURN:
    codegen_stmt(stmt->lhs);
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
    break;
  case COMPOUND_STMT:
    cur = stmt->stmts;
    while (cur) {
      codegen_stmt(cur);
      cur = cur->next;
    }
    break;
  default:
    not_implemented();
    break;
  }
}
