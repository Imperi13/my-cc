
#include <stdio.h>

#include "codegen.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void codegen_function(Tree *func);
static void codegen_stmt(Tree *stmt);
static void codegen_addr(Tree *stmt);

static void load2rax_from_raxaddr(Type *type);

char call_register64[][4] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
char call_register32[][4] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
char call_register8[][4] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};

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
  printf("  sub rsp, %d\n", calc_rbp_offset(0, func->def_obj->stack_size, 8));

  Tree *cur = getargs_declarator(func->declarator);
  int count = 0;
  while (cur) {
    printf("  mov [rbp - %d], %s\n", cur->def_obj->rbp_offset,
           call_register32[count]);
    count++;
    cur = cur->next;
  }

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
  } else if (stmt->kind == DEREF) {
    codegen_stmt(stmt->lhs);
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
  case BREAK:
    printf("  jmp .Lend%d\n", stmt->label_number);
    return;
  case CONTINUE:
    printf("  jmp .Lloopend%d\n", stmt->label_number);
    return;
  case COMPOUND_STMT:
    cur = stmt->stmts;
    while (cur) {
      codegen_stmt(cur);
      cur = cur->next;
    }
    return;
  case WHILE:
    printf(".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(stmt->cond);
    printf("  cmp rax,0\n");
    printf("  je .Lend%d\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    printf(".Lloopend%d:\n", stmt->label_number);
    printf("  jmp .Lbegin%d\n", stmt->label_number);
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case DO_WHILE:
    printf(".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    printf(".Lloopend%d:\n", stmt->label_number);
    codegen_stmt(stmt->cond);
    printf("  cmp rax,0\n");
    printf("  jne .Lbegin%d\n", stmt->label_number);
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case FOR:
    if (stmt->for_init)
      codegen_stmt(stmt->for_init);
    printf(".Lbegin%d:\n", stmt->label_number);
    codegen_stmt(stmt->cond);
    printf("  cmp rax,0\n");
    printf("  je .Lend%d\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    printf(".Lloopend%d:\n", stmt->label_number);
    if (stmt->for_update)
      codegen_stmt(stmt->for_update);
    printf("  jmp .Lbegin%d\n", stmt->label_number);
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case IF:
    codegen_stmt(stmt->cond);
    printf("  cmp rax,0\n");
    printf("  je .Lelse%d\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    printf("  jmp .Lend%d\n", stmt->label_number);
    printf(".Lelse%d:\n", stmt->label_number);
    if (stmt->rhs)
      codegen_stmt(stmt->rhs);
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  pop rdi\n");
    printf("  mov [rdi],eax\n");
    return;
  case ADD_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  add rax, rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case SUB_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  sub rax, rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case MUL_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  imul rax, rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case DIV_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  cqo\n");
    printf("  idiv rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case MOD_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  cqo\n");
    printf("  idiv rdi\n");
    printf("  mov rax,rdx\n");
    printf("  mov [rsi], eax\n");
    return;
  case AND_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  and rax, rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case OR_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  or rax, rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case XOR_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  xor rax, rdi\n");
    printf("  mov [rsi], eax\n");
    return;
  case LSHIFT_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  mov rcx, rdi\n");
    printf("  sal rax, cl\n");
    printf("  mov [rsi], eax\n");
    return;
  case RSHIFT_ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rsi\n");
    printf("  movsxd rax,[rsi]\n");
    printf("  mov rcx, rdi\n");
    printf("  sar rax, cl\n");
    printf("  mov [rsi], eax\n");
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
  case LOGICAL_OR:
    codegen_stmt(stmt->lhs);
    printf("  cmp rax,0\n");
    printf("  jne .Ltrue%d\n", stmt->label_number);
    codegen_stmt(stmt->rhs);
    printf("  cmp rax,0\n");
    printf("  jne .Ltrue%d\n", stmt->label_number);
    printf("  mov rax, 0\n");
    printf("  jmp .Lend%d\n", stmt->label_number);
    printf(".Ltrue%d:\n", stmt->label_number);
    printf("  mov rax, 1\n");
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case LOGICAL_AND:
    codegen_stmt(stmt->lhs);
    printf("  cmp rax,0\n");
    printf("  je .Lfalse%d\n", stmt->label_number);
    codegen_stmt(stmt->rhs);
    printf("  cmp rax,0\n");
    printf("  je .Lfalse%d\n", stmt->label_number);
    printf("  mov rax, 1\n");
    printf("  jmp .Lend%d\n", stmt->label_number);
    printf(".Lfalse%d:\n", stmt->label_number);
    printf("  mov rax, 0\n");
    printf(".Lend%d:\n", stmt->label_number);
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
  case ADDR:
    codegen_addr(stmt->lhs);
    return;
  case DEREF:
    codegen_stmt(stmt->lhs);
    load2rax_from_raxaddr(stmt->type);
    return;
  case FUNC_CALL: {
    printf("  mov r10,rsp\n");
    printf("  and rsp,0xfffffffffffffff0\n");
    printf("  push r10\n");
    printf("  push 0\n");
    int stack_count = 0;
    Tree *cur = stmt->call_args;
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
  case POST_INCREMENT:
    codegen_addr(stmt->lhs);
    printf("  mov rdi,rax\n");
    load2rax_from_raxaddr(stmt->lhs->type);
    printf("  mov rsi,rax\n");
    printf("  add rsi, 1\n");
    printf("  mov [rdi],esi\n");
    return;
  case POST_DECREMENT:
    codegen_addr(stmt->lhs);
    printf("  mov rdi,rax\n");
    load2rax_from_raxaddr(stmt->lhs->type);
    printf("  mov rsi,rax\n");
    printf("  sub rsi, 1\n");
    printf("  mov [rdi],esi\n");
    return;
  case NUM:
    printf("  mov rax, %ld\n", stmt->num);
    return;
  case VAR:
    codegen_addr(stmt);
    if (stmt->type->kind == FUNC)
      return;
    load2rax_from_raxaddr(stmt->type);
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

// raxレジスタで指しているアドレスからtype型の値をraxにロードする
void load2rax_from_raxaddr(Type *type) {
  if (type_size(type) == 8)
    printf("  mov rax,[rax]\n");
  else if (type_size(type) == 4)
    printf("  movsxd rax,[rax]\n");
  else
    not_implemented();
}
