#include "mycc.h"

int label_count = 0;

void gen_lval(Node *node) {
  if (node->kind != ND_LVAR)
    error("not lval");

  printf("  mov rax, rbp\n");
  printf("  sub rax, %d\n",node->offset);
  printf("  push rax\n");
}

void gen(Node *node) {
  int now_count;
  switch(node->kind) {
    case ND_NUM:
      printf("  push %d\n",node->val);
      return;
    case ND_LVAR:
      gen_lval(node);
      printf("  pop rax\n");
      printf("  mov rax, [rax]\n");
      printf("  push rax\n");
      return ;
    case ND_ASSIGN:
      gen_lval(node->lhs);
      gen(node->rhs);

      printf("  pop rdi\n");
      printf("  pop rax\n");
      printf("  mov [rax], rdi\n");
      printf("  push rdi\n");
      return;
    case ND_RETURN:
      gen(node->lhs);
      printf("  pop rax\n");
      printf("  mov rsp,rbp\n");
      printf("  pop rbp\n");
      printf("  ret\n");
      return;
    case ND_IF:
      now_count = label_count;
      label_count++;
      gen(node->expr);
      printf("  pop rax\n");
      printf("  cmp rax,0\n");
      printf("  je .Lend%d\n",now_count);
      gen(node->lhs);
      printf(".Lend%d:\n",now_count);
      return;
    case ND_IFELSE:
      now_count = label_count;
      label_count++;
      gen(node->expr);
      printf("  pop rax\n");
      printf("  cmp rax,0\n");
      printf("  je .Lelse%d\n",now_count);
      gen(node->lhs);
      printf("  jmp .Lend%d\n",now_count);
      printf(".Lelse%d:\n",now_count);
      gen(node->rhs);
      printf(".Lend%d:\n",now_count);
      return;
    case ND_WHILE:
      now_count = label_count;
      label_count++;
      printf(".Lbegin%d:\n",now_count);
      gen(node->expr);
      printf("  pop rax\n");
      printf("  cmp rax,0\n");
      printf("  je .Lend%d\n",now_count);
      gen(node->lhs);
      printf("  jmp .Lbegin%d\n",now_count);
      printf(".Lend%d:\n",now_count);
      return;
    case ND_FOR:
      now_count = label_count;
      label_count++;
      if(node->init_expr)
        gen(node->init_expr);
      printf(".Lbegin%d:\n",now_count);
      gen(node->expr);
      printf("  pop rax\n");
      printf("  cmp rax,0\n");
      printf("  je .Lend%d\n",now_count);
      gen(node->lhs);
      if(node->update_expr)
        gen(node->update_expr);
      printf("  jmp .Lbegin%d\n",now_count);
      printf(".Lend%d:\n",now_count);
      return;
    case ND_BLOCK:
      while(node->front){
        gen(node->front->stmt);
        node->front = node->front->next;
        if(node->front)
          printf("  pop rax\n");
      }
      return;
    case ND_FUNCTION_CALL:
      printf("  mov r8,rsp\n");
      printf("  and rsp,0xfffffffffffffff0\n");
      printf("  push r8\n");
      printf("  push 0\n");
      printf("  call %.*s\n",node->func_name_len,node->func_name);
      printf("  pop r8\n");
      printf("  pop rsp\n");
      printf("  push rax\n");
      return;
    default:
      break;
  }

  gen(node->lhs);
  gen(node->rhs);

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (node->kind) {
    case ND_ADD:
      printf("  add rax,rdi\n");
      break;
    case ND_SUB:
      printf("  sub rax,rdi\n");
      break;
    case ND_MUL:
      printf("  imul rax,rdi\n");
      break;
    case ND_DIV:
      printf("  cqo\n");
      printf("  idiv rdi\n");
      break;
    case ND_EQUAL:
      printf("  cmp rax,rdi\n");
      printf("  sete al\n");
      printf("  movzb rax,al\n");
      break;
    case ND_NOT_EQUAL:
      printf("  cmp rax,rdi\n");
      printf("  setne al\n");
      printf("  movzb rax,al\n");
      break;
    case ND_SMALLER:
      printf("  cmp rax,rdi\n");
      printf("  setl al\n");
      printf("  movzb rax,al\n");
      break;
    case ND_SMALLER_EQUAL:
      printf("  cmp rax,rdi\n");
      printf("  setle al\n");
      printf("  movzb rax,al\n");
      break;
    case ND_GREATER:
      printf("  cmp rdi,rax\n");
      printf("  setl al\n");
      printf("  movzb rax,al\n");
      break;
    case ND_GREATER_EQUAL:
      printf("  cmp rdi,rax\n");
      printf("  setle al\n");
      printf("  movzb rax,al\n");
      break;
    default:
      error("invalid op");
  }

  printf("  push rax\n");
}

void codegen_all(FILE *output) {
  int len = 0;
  LVar *now = locals;
  while(now){
    len++;
    now = now->next;
  }

  fprintf(output,".intel_syntax noprefix\n");
  fprintf(output,".globl main\n");
  fprintf(output,"main:\n");
 
  fprintf(output,"  push rbp\n");
  fprintf(output,"  mov rbp, rsp\n");
  fprintf(output,"  sub rsp, %d\n",len*8);

  for (StmtList *now = code_front;now;now = now->next) {
    gen(now->stmt);
    fprintf(output,"  pop rax\n");
  }

  fprintf(output,"  mov rsp, rbp\n");
  fprintf(output,"  pop rbp\n");
  fprintf(output,"  ret\n");
}
