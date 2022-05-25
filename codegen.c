#include "mycc.h"

char call_register64[][4] = {"rdi","rsi","rdx","rcx","r8","r9"};
char call_register32[][4] = {"edi","esi","edx","ecx","r8d","r9d"};

int label_count = 0;


void gen_lval(Node *node) {
  if (node->kind != ND_LVAR && node->kind != ND_DEREF)
    error("not lval");

  if(node->kind == ND_DEREF){
    gen(node->lhs);
    return;
  }

  printf("  mov rax, rbp\n");
  printf("  sub rax, %d\n",node->offset);
  printf("  push rax\n");
}

void gen(Node *node) {
  int now_count;
  int arg_count;
  switch(node->kind) {
    case ND_NUM:
      printf("  push %d\n",node->val);
      return;
    case ND_LVAR_DEFINE:
      printf("  push rax\n");
      return;
    case ND_LVAR:
      gen_lval(node);
      if(node->type->ty == ARRAY)
        return;
      printf("  pop rax\n");
      if(type_size(node->type) == 8)
        printf("  mov rax, [rax]\n");
      else if(type_size(node->type) == 4)
        printf("  mov eax, [rax]\n");
      printf("  push rax\n");
      return ;
    case ND_ASSIGN:
      gen_lval(node->lhs);
      gen(node->rhs);

      printf("  pop rdi\n");
      printf("  pop rax\n");
      if(type_size(node->rhs->type) == 8)
        printf("  mov [rax], rdi\n");
      else if(type_size(node->rhs->type) == 4)
        printf("  mov [rax], edi\n");
      printf("  push rdi\n");
      return;
    case ND_ADDR:
      gen_lval(node->lhs);
      return;
    case ND_DEREF:
      gen(node->lhs);
      printf("  pop rax\n");
      if(type_size(node->type) == 8)
        printf("  mov rax, [rax]\n");
      else if(type_size(node->type) == 4)
        printf("  mov eax, [rax]\n");
      printf("  push rax\n");
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
      while(node->stmt_front){
        gen(node->stmt_front->node);
        node->stmt_front = node->stmt_front->next;
        if(node->stmt_front)
          printf("  pop rax\n");
      }
      return;
    case ND_FUNCTION_CALL:
      printf("  mov r8,rsp\n");
      printf("  and rsp,0xfffffffffffffff0\n");
      printf("  push r8\n");
      printf("  push 0\n");
      arg_count = 0;
      while(node->expr_front){
        if(arg_count >= 6)
          error("more than 6 arguments is not implemented");
        gen(node->expr_front->node);
        node->expr_front = node->expr_front->next;
        arg_count++;
      }
      for(int i=arg_count-1;i>=0;i--){
        printf("  pop %s\n",call_register64[i]);
      }
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
      if(node->lhs->type->ty == PTR || node->lhs->type->ty == ARRAY)
        printf("  imul rdi, %d\n",type_size(node->lhs->type->ptr_to));
      else if(node->rhs->type->ty == PTR || node->rhs->type->ty == ARRAY)
        printf("  imul rax, %d\n",type_size(node->rhs->type->ptr_to));
      printf("  add rax,rdi\n");
      break;
    case ND_SUB:
      if(node->lhs->type->ty == PTR || node->lhs->type->ty == ARRAY)
        printf(" imul rdi, %d\n",type_size(node->lhs->type->ptr_to));
      printf("  sub rax,rdi\n");
      break;
    case ND_MUL:
      printf("  imul rax,rdi\n");
      break;
    case ND_DIV:
      printf("  cqo\n");
      printf("  idiv rdi\n");
      break;
    case ND_MOD:
      printf("  cqo\n");
      printf("  idiv rdi\n");
      printf("  mov rax, rdx\n");
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

void gen_function(Function *func) {
  int stack_offset = 0;
  if(func->locals)
    stack_offset = func->locals->lvar->offset;

  printf(".globl %.*s\n",func->func_name_len,func->func_name);
  printf("%.*s:\n",func->func_name_len,func->func_name);

  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n",offset_alignment(0,stack_offset,8));

  LVarList *now_arg = func->arg_front;
  for(int i = 0;i < func->arg_count;i++){
    printf(" mov rax, rbp\n");
    printf("  sub rax, %d\n",now_arg->lvar->offset);
    if(type_size(now_arg->lvar->type) == 8)
      printf("  mov [rax], %s\n",call_register64[i]);
    else if(type_size(now_arg->lvar->type) == 4)
      printf("  mov [rax], %s\n",call_register32[i]);
    now_arg = now_arg->next;
  }

  for(NodeList *stmt = func->code_front;stmt;stmt = stmt->next) {
    gen(stmt->node);
    printf("  pop rax\n");
  }

  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}

void codegen_all(FILE *output) {
  fprintf(output,".intel_syntax noprefix\n");
  
  for(Function *now = functions;now;now = now->next){
    gen_function(now);
  }
}
