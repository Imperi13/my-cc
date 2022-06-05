#include "mycc.h"

void gen(Node *node);

char call_register64[][4] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
char call_register32[][4] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
char call_register8[][4] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};

typedef struct LoopScope LoopScope;

struct LoopScope {
  LoopScope *next;
  int label_num;
};

char *rax_register(Type *a) {
  int size = type_size(a);
  if (size == 8)
    return "rax";
  else if (size == 4)
    return "eax";
  else if (size == 2)
    return "ax";
  else if (size == 1)
    return "al";

  error("invalid type size");
  return NULL;
}

char *rdi_register(Type *a) {
  int size = type_size(a);
  if (size == 8)
    return "rdi";
  else if (size == 4)
    return "edi";
  else if (size == 2)
    return "di";
  else if (size == 1)
    return "dil";

  error("invalid type size");
  return NULL;
}

int label_count = 0;
LoopScope *loop_scope = NULL;

void gen_addr(Node *node) {
  if (node->kind != ND_VAR && node->kind != ND_DEREF)
    error("not lval");

  if (node->kind == ND_DEREF) {
    gen(node->lhs);
    return;
  }

  if (node->type->ty == FUNC) {
    if (node->is_defined)
      printf("  lea rax, [rip + %.*s]\n", node->len, node->name);
    else
      printf("  mov rax, [%.*s@GOTPCREL + rip]\n", node->len, node->name);
    return;
  }

  if (node->is_global) {
    printf("  lea rax, [rip + %.*s]\n", node->len, node->name);
    return;
  }

  printf("  mov rax, rbp\n");
  printf("  sub rax, %d\n", node->offset);
}

void gen(Node *node) {
  int now_count;
  int arg_count;
  LoopScope *loop;
  switch (node->kind) {
  case ND_NOP:
    printf("  mov rax, 0\n");
    return;
  case ND_NUM:
    printf("  mov rax, %d\n", node->val);
    return;
  case ND_LOGICAL_NOT:
    gen(node->lhs);
    printf("  cmp rax,0\n");
    printf("  sete al\n");
    printf("  movsx rax,al\n");
    return;
  case ND_BIT_NOT:
    gen(node->lhs);
    printf("  not rax\n");
    return;
  case ND_VAR:
    gen_addr(node);
    if (node->type->ty == ARRAY || node->type->ty == FUNC)
      return;
    if (type_size(node->type) == 8)
      printf("  mov rax, [rax]\n");
    else if (type_size(node->type) == 4)
      printf("  movsxd rax, [rax]\n");
    else if (type_size(node->type) == 1)
      printf("  movsx rax, BYTE PTR [rax]\n");
    return;
  case ND_STR:
    printf("  lea rax, [rip + .LC%d]\n", node->str_literal->id);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    printf("  push rax\n");
    gen(node->rhs);
    printf("  push rax\n");

    printf("  pop rdi\n");
    printf("  pop rax\n");
    if (type_size(node->lhs->type) == 8)
      printf("  mov [rax], rdi\n");
    else if (type_size(node->lhs->type) == 4)
      printf("  mov [rax], edi\n");
    else if (type_size(node->lhs->type) == 1)
      printf("  mov [rax], dil\n");
    printf("  mov rax, rdi\n");
    return;
  case ND_ADD_ASSIGN:
    gen_addr(node->lhs);
    printf("  push rax\n");
    gen(node->rhs);
    printf("  mov rdi,rax\n");
    printf("  pop rax\n");
    if (type_size(node->lhs->type) == 8)
      printf("  mov rsi, [rax]\n");
    else if (type_size(node->lhs->type) == 4)
      printf("  movsxd rsi, [rax]\n");
    else if (type_size(node->lhs->type) == 1)
      printf("  movsx rsi, BYTE PTR [rax]\n");
    if (node->lhs->type->ty == PTR || node->lhs->type->ty == ARRAY)
      printf("  imul rdi, %d\n", type_size(node->lhs->type->ptr_to));
    else if (node->rhs->type->ty == PTR || node->rhs->type->ty == ARRAY)
      printf("  imul rsi, %d\n", type_size(node->rhs->type->ptr_to));
    printf("  add rdi,rsi\n");
    if (type_size(node->lhs->type) == 8)
      printf("  mov [rax], rdi\n");
    else if (type_size(node->lhs->type) == 4)
      printf("  mov [rax], edi\n");
    else if (type_size(node->lhs->type) == 1)
      printf("  mov [rax], dil\n");
    printf("  mov rax,rdi\n");
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_DEREF:
    gen(node->lhs);
    if (node->type->ty == ARRAY)
      return;
    if (type_size(node->type) == 8)
      printf("  mov rax, [rax]\n");
    else if (type_size(node->type) == 4)
      printf("  movsxd rax, [rax]\n");
    else if (type_size(node->type) == 1)
      printf("  movsx rax, BYTE PTR [rax]\n");
    return;
  case ND_RETURN:
    gen(node->lhs);
    printf("  mov rsp,rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
    return;
  case ND_BREAK:
    if (!loop_scope)
      error("not in loop");
    printf("  jmp .Lend%d\n", loop_scope->label_num);
    return;
  case ND_CONTINUE:
    if (!loop_scope)
      error("not in loop");
    printf("  jmp .Lloopend%d\n", loop_scope->label_num);
    return;
  case ND_LOGICAL_AND:
    now_count = label_count;
    label_count++;
    gen(node->lhs);
    printf("  cmp rax,0\n");
    printf("  je .Lfalse%d\n", now_count);
    gen(node->rhs);
    printf("  cmp rax,0\n");
    printf("  je .Lfalse%d\n", now_count);
    printf("  mov rax, 1\n");
    printf("  jmp .Lend%d\n", now_count);
    printf(".Lfalse%d:\n", now_count);
    printf("  mov rax, 0\n");
    printf(".Lend%d:\n", now_count);
    return;
  case ND_LOGICAL_OR:
    now_count = label_count;
    label_count++;
    gen(node->lhs);
    printf("  cmp rax,0\n");
    printf("  jne .Ltrue%d\n", now_count);
    gen(node->rhs);
    printf("  cmp rax,0\n");
    printf("  jne .Ltrue%d\n", now_count);
    printf("  mov rax, 0\n");
    printf("  jmp .Lend%d\n", now_count);
    printf(".Ltrue%d:\n", now_count);
    printf("  mov rax, 1\n");
    printf(".Lend%d:\n", now_count);
    return;
  case ND_CONDITIONAL:
    now_count = label_count;
    label_count++;
    gen(node->expr);
    printf("  cmp rax,0\n");
    printf("  jne .Ltrue%d\n", now_count);
    printf("  jmp .Lfalse%d\n", now_count);
    printf(".Ltrue%d:\n", now_count);
    gen(node->lhs);
    printf("  jmp .Lend%d\n", now_count);
    printf(".Lfalse%d:\n", now_count);
    gen(node->rhs);
    printf(".Lend%d:\n", now_count);
    return;
  case ND_IF:
    now_count = label_count;
    label_count++;
    gen(node->expr);
    printf("  cmp rax,0\n");
    printf("  je .Lend%d\n", now_count);
    gen(node->lhs);
    printf(".Lend%d:\n", now_count);
    return;
  case ND_IFELSE:
    now_count = label_count;
    label_count++;
    gen(node->expr);
    printf("  cmp rax,0\n");
    printf("  je .Lelse%d\n", now_count);
    gen(node->lhs);
    printf("  jmp .Lend%d\n", now_count);
    printf(".Lelse%d:\n", now_count);
    gen(node->rhs);
    printf(".Lend%d:\n", now_count);
    return;
  case ND_DO_WHILE:
    now_count = label_count;
    label_count++;

    loop = calloc(1, sizeof(LoopScope));
    loop->label_num = now_count;
    loop->next = loop_scope;
    loop_scope = loop;

    printf(".Lbegin%d:\n", now_count);
    gen(node->lhs);
    printf(".Lloopend%d:\n", now_count);
    gen(node->expr);
    printf("  cmp rax,0\n");
    printf("  jne .Lbegin%d\n", now_count);
    printf(".Lend%d:\n", now_count);
    loop_scope = loop_scope->next;
    return;
  case ND_WHILE:
    now_count = label_count;
    label_count++;

    loop = calloc(1, sizeof(LoopScope));
    loop->label_num = now_count;
    loop->next = loop_scope;
    loop_scope = loop;

    printf(".Lbegin%d:\n", now_count);
    gen(node->expr);
    printf("  cmp rax,0\n");
    printf("  je .Lend%d\n", now_count);
    gen(node->lhs);
    printf(".Lloopend%d:\n", now_count);
    printf("  jmp .Lbegin%d\n", now_count);
    printf(".Lend%d:\n", now_count);
    loop_scope = loop_scope->next;
    return;
  case ND_FOR:
    now_count = label_count;
    label_count++;

    loop = calloc(1, sizeof(LoopScope));
    loop->label_num = now_count;
    loop->next = loop_scope;
    loop_scope = loop;

    if (node->init_expr)
      gen(node->init_expr);
    printf(".Lbegin%d:\n", now_count);
    gen(node->expr);
    printf("  cmp rax,0\n");
    printf("  je .Lend%d\n", now_count);
    gen(node->lhs);
    printf(".Lloopend%d:\n", now_count);
    if (node->update_expr)
      gen(node->update_expr);
    printf("  jmp .Lbegin%d\n", now_count);
    printf(".Lend%d:\n", now_count);
    loop_scope = loop_scope->next;
    return;
  case ND_BLOCK:
    while (node->stmt_front) {
      gen(node->stmt_front->node);
      node->stmt_front = node->stmt_front->next;
    }
    return;
  case ND_COMMA:
    gen(node->lhs);
    gen(node->rhs);
    return;
  case ND_FUNCTION_CALL:
    printf("  mov r8,rsp\n");
    printf("  and rsp,0xfffffffffffffff0\n");
    printf("  push r8\n");
    printf("  push 0\n");
    arg_count = 0;
    while (node->expr_front) {
      if (arg_count >= 6)
        error("more than 6 arguments is not implemented");
      gen(node->expr_front->node);
      printf("  push rax\n");
      node->expr_front = node->expr_front->next;
      arg_count++;
    }
    for (int i = arg_count - 1; i >= 0; i--) {
      printf("  pop %s\n", call_register64[i]);
    }
    gen_addr(node->lhs);
    printf("  mov r10,rax\n");
    // printf("  call %.*s\n",node->len,node->name);
    printf("  call r10\n");
    printf("  pop r8\n");
    printf("  pop rsp\n");
    return;
  default:
    break;
  }

  gen(node->lhs);
  printf("  push rax\n");
  gen(node->rhs);
  printf("  push rax\n");

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (node->kind) {
  case ND_ADD:
    if (node->lhs->type->ty == PTR || node->lhs->type->ty == ARRAY)
      printf("  imul rdi, %d\n", type_size(node->lhs->type->ptr_to));
    else if (node->rhs->type->ty == PTR || node->rhs->type->ty == ARRAY)
      printf("  imul rax, %d\n", type_size(node->rhs->type->ptr_to));
    printf("  add rax,rdi\n");
    break;
  case ND_SUB:
    if ((node->lhs->type->ty == PTR || node->lhs->type->ty == ARRAY) &&
        is_numeric(node->rhs->type))
      printf(" imul rdi, %d\n", type_size(node->lhs->type->ptr_to));
    printf("  sub rax,rdi\n");
    if (node->lhs->type->ty == PTR && node->rhs->type->ty == PTR) {
      printf("  mov rdi, %d\n", type_size(node->lhs->type->ptr_to));
      printf("  cqo\n");
      printf("  idiv rdi\n");
    }
    break;
  case ND_MUL:
    printf("  imul rax,rdi\n");
    break;
  case ND_DIV:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    break;
  case ND_LSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  sal rax, cl\n");
    break;
  case ND_RSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  sar rax, cl\n");
    break;
  case ND_BIT_AND:
    printf("  and rax, rdi\n");
    break;
  case ND_BIT_XOR:
    printf("  xor rax, rdi\n");
    break;
  case ND_BIT_OR:
    printf("  or rax, rdi\n");
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
}

void gen_function(Obj *func) {
  int stack_offset = 0;
  loop_scope = NULL;

  printf("  .text\n");
  printf(".globl %.*s\n", func->len, func->name);
  printf("%.*s:\n", func->len, func->name);

  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", offset_alignment(0, func->stack_size, 8));

  ObjList *now_arg = func->arg_front;
  for (int i = 0; i < func->arg_size; i++) {
    printf(" mov rax, rbp\n");
    printf("  sub rax, %d\n", now_arg->obj->offset);
    if (type_size(now_arg->obj->type) == 8)
      printf("  mov [rax], %s\n", call_register64[i]);
    else if (type_size(now_arg->obj->type) == 4)
      printf("  mov [rax], %s\n", call_register32[i]);
    else if (type_size(now_arg->obj->type) == 1)
      printf("  mov [rax], %s\n", call_register8[i]);
    now_arg = now_arg->next;
  }

  gen(func->code);

  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}

void gen_var_definition(Obj *var) {
  printf("  .globl %.*s\n", var->len, var->name);
  printf("  .bss\n");
  printf("  .align %d\n", type_alignment(var->type));
  printf("%.*s:\n", var->len, var->name);
  printf("  .zero %d\n", type_size(var->type));
}

void gen_str_literal(StrLiteral *str_literal) {
  printf("  .globl .LC%d\n", str_literal->id);
  printf("  .data\n");
  printf(".LC%d:\n", str_literal->id);
  printf("  .string \"%.*s\"\n", str_literal->len, str_literal->str);
}

void codegen_all(FILE *output) {
  label_count = 0;
  fprintf(output, ".intel_syntax noprefix\n");

  for (StrLiteral *now = str_literals; now; now = now->next) {
    gen_str_literal(now);
  }

  for (ObjList *now = globals; now; now = now->next) {
    if (now->obj->type->ty != FUNC)
      gen_var_definition(now->obj);
  }

  for (ObjList *now = globals; now; now = now->next) {
    if (now->obj->type->ty == FUNC && now->obj->is_defined)
      gen_function(now->obj);
  }
}
