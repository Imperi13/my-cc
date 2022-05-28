#include "mycc.h"

void gen(Node *node);

char call_register64[][4] = {"rdi","rsi","rdx","rcx","r8","r9"};
char call_register32[][4] = {"edi","esi","edx","ecx","r8d","r9d"};
char call_register8[][4] = {"dil","sil","dl","cl","r8b","r9b"};

int label_count = 0;


void gen_lval(Node *node) {
  if (node->kind != ND_VAR && node->kind != ND_DEREF)
    error("not lval");

  if(node->kind == ND_DEREF){
    gen(node->lhs);
    return;
  }

  if(node->type->ty == FUNC){
    if(node->is_defined)
      printf("  lea rax, [rip + %.*s]\n",node->len,node->name);
    else
      printf("  mov rax, [%.*s@GOTPCREL + rip]\n",node->len,node->name);
    printf("  push rax\n");
    return;
  }

  if(node->is_global){
    printf("  lea rax, [rip + %.*s]\n",node->len,node->name);
    printf("  push rax\n");
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
    case ND_VAR_DEFINE:
      printf("  push rax\n");
      return;
    case ND_VAR:
      gen_lval(node);
      if(node->type->ty == ARRAY || node->type->ty == FUNC)
        return;
      printf("  pop rax\n");
      if(type_size(node->type) == 8)
        printf("  mov rax, [rax]\n");
      else if(type_size(node->type) == 4)
        printf("  mov eax, [rax]\n");
      else if(type_size(node->type) == 1)
        printf("  movsx eax, BYTE PTR [rax]\n");
      printf("  push rax\n");
      return ;
    case ND_ASSIGN:
      gen_lval(node->lhs);
      gen(node->rhs);

      printf("  pop rdi\n");
      printf("  pop rax\n");
      if(type_size(node->lhs->type) == 8)
        printf("  mov [rax], rdi\n");
      else if(type_size(node->lhs->type) == 4)
        printf("  mov [rax], edi\n");
      else if(type_size(node->lhs->type) == 1)
        printf("  mov [rax], dil\n");
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
      else if(type_size(node->type) == 1)
        printf("  movsx eax, BYTE PTR [rax]\n");
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
      gen_lval(node->lhs);
      printf("  pop r10\n");
      // printf("  call %.*s\n",node->len,node->name);
      printf("  call r10\n");
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

void gen_function(Obj *func) {
  int stack_offset = 0;
  if(func->locals)
    stack_offset = func->locals->obj->offset;

  printf("  .text\n");
  printf(".globl %.*s\n",func->len,func->name);
  printf("%.*s:\n",func->len,func->name);

  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n",offset_alignment(0,stack_offset,8));

  ObjList *now_arg = func->arg_front;
  for(int i = 0;i < func->arg_size;i++){
    printf(" mov rax, rbp\n");
    printf("  sub rax, %d\n",now_arg->obj->offset);
    if(type_size(now_arg->obj->type) == 8)
      printf("  mov [rax], %s\n",call_register64[i]);
    else if(type_size(now_arg->obj->type) == 4)
      printf("  mov [rax], %s\n",call_register32[i]);
    else if(type_size(now_arg->obj->type) == 1)
      printf("  mov [rax], %s\n",call_register8[i]);
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

void gen_var_definition(Obj *var) {
  printf("  .globl %.*s\n",var->len,var->name);
  printf("  .bss\n");
  printf("  .align %d\n",type_alignment(var->type));
  printf("%.*s:\n",var->len,var->name);
  printf("  .zero %d\n",type_size(var->type));
}

void codegen_all(FILE *output) {
  fprintf(output,".intel_syntax noprefix\n");
 
  for(ObjList *now = globals;now;now = now->next){
    if(now->obj->type->ty != FUNC)
      gen_var_definition(now->obj);
  }

  for(ObjList *now = globals;now;now = now->next){
    if(now->obj->type->ty == FUNC && now->obj->is_defined)
      gen_function(now->obj);
  }
}
