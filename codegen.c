
#include <stdio.h>

#include "codegen.h"
#include "error.h"
#include "parse.h"
#include "type.h"

static void codegen_str_literal(StrLiteral *sl);
static void codegen_var_definition(Tree *var);
static void codegen_function(Tree *func);
static void codegen_stmt(Tree *stmt);
static void codegen_addr(Tree *stmt);

static void load2rax_from_raxaddr(Type *type);
static void store2rdiaddr_from_rax(Type *type);

char call_register64[][4] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
char call_register32[][4] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
char call_register8[][4] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};

void codegen_translation_unit(Tree *head) {

  printf(".intel_syntax noprefix\n");

  for (StrLiteral *now = str_literals; now; now = now->next) {
    codegen_str_literal(now);
  }

  Tree *cur = head;
  while (cur) {
    if (cur->kind == DECLARATION && cur->declarator &&
        cur->def_obj->is_defined) {
      codegen_var_definition(cur);
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

void codegen_str_literal(StrLiteral *sl) {
  printf("  .local .LC%d\n", sl->id);
  printf("  .data\n");
  printf(".LC%d:\n", sl->id);
  //  printf("  .string \"%.*s\"\n", sl->len, sl->str);
  printf("  .string \"");
  for (int i = 0; i < sl->len; i++) {
    if (sl->str[i] == '\e') {
      printf("\\033");
    } else if (sl->str[i] == '\n') {
      printf("\\n");
    } else {
      printf("%c", sl->str[i]);
    }
  }
  printf("\"\n");
}

void codegen_var_definition(Tree *var) {
  Obj *obj = var->def_obj;
  printf("  .globl %.*s\n", obj->obj_len, obj->obj_name);
  printf("  .bss\n");
  printf("  .align %d\n", type_alignment(obj->type));
  printf("%.*s:\n", obj->obj_len, obj->obj_name);
  printf("  .zero %d\n", type_size(obj->type));
}

void codegen_function(Tree *func) {

  printf("  .text\n");
  printf(".globl %.*s\n", func->def_obj->obj_len, func->def_obj->obj_name);
  printf("%.*s:\n", func->def_obj->obj_len, func->def_obj->obj_name);

  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", calc_rbp_offset(0, func->def_obj->stack_size, 8));

  Tree *cur = getargs_declarator(func->declarator);
  int count = 0;
  while (cur) {
    if (type_size(cur->def_obj->type) == 8)
      printf("  mov [rbp - %d], %s\n", cur->def_obj->rbp_offset,
             call_register64[count]);
    else if (type_size(cur->def_obj->type) == 4)
      printf("  mov [rbp - %d], %s\n", cur->def_obj->rbp_offset,
             call_register32[count]);
    else if (type_size(cur->def_obj->type) == 1)
      printf("  mov [rbp - %d], %s\n", cur->def_obj->rbp_offset,
             call_register8[count]);
    else
      not_implemented(__func__);
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
  } else if (stmt->kind == DOT) {
    codegen_addr(stmt->lhs);
    printf("  add rax, %d\n", stmt->member->offset);
  } else if (stmt->kind == ARROW) {
    codegen_stmt(stmt->lhs);
    printf("  add rax, %d\n", stmt->member->offset);
  } else {
    not_implemented(__func__);
  }
}

void codegen_stmt(Tree *stmt) {
  Tree *cur;
  switch (stmt->kind) {
  case DECLARATION:
    if (stmt->declarator && stmt->declarator->init_expr) {
      printf("  lea rdi, [rbp - %d]\n", stmt->def_obj->rbp_offset);
      codegen_stmt(stmt->declarator->init_expr);
      store2rdiaddr_from_rax(stmt->def_obj->type);
    }
    return;
  case LABEL:
    printf(".Llabel%.*s:\n", stmt->label_len, stmt->label_name);
    codegen_stmt(stmt->lhs);
    return;
  case CASE:
    printf(".Lswitch%d_case%d:\n", stmt->label_number, stmt->case_num);
    codegen_stmt(stmt->lhs);
    return;
  case DEFAULT:
    printf(".Lswitch%d_default:\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    return;
  case RETURN:
    if (stmt->lhs)
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
  case SWITCH:
    codegen_stmt(stmt->cond);
    for (Case *cur = stmt->cases; cur; cur = cur->next) {
      printf("  cmp rax, %d\n", cur->case_num);
      printf("  je .Lswitch%d_case%d\n", stmt->label_number, cur->case_num);
    }
    if (stmt->has_default)
      printf("  jmp .Lswitch%d_default\n", stmt->label_number);
    printf("  jmp .Lend%d\n", stmt->label_number);
    codegen_stmt(stmt->lhs);
    printf(".Lend%d:\n", stmt->label_number);
    return;
  case ASSIGN:
    codegen_addr(stmt->lhs);
    printf("  push rax\n");
    codegen_stmt(stmt->rhs);
    printf("  pop rdi\n");
    store2rdiaddr_from_rax(stmt->type);
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
    if (stmt->type->kind == ARRAY)
      return;
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
  case DOT:
    codegen_addr(stmt);
    load2rax_from_raxaddr(stmt->type);
    return;
  case ARROW:
    codegen_addr(stmt);
    load2rax_from_raxaddr(stmt->type);
    return;
  case NUM:
    printf("  mov rax, %ld\n", stmt->num);
    return;
  case STR:
    printf("  lea rax, [rip + .LC%d]\n", stmt->str_literal->id);
    return;
  case VAR:
    codegen_addr(stmt);
    if (stmt->type->kind == FUNC || stmt->type->kind == ARRAY)
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
    if (stmt->lhs->type->kind == PTR || stmt->lhs->type->kind == ARRAY)
      printf("  imul rdi,%d\n", type_size(stmt->lhs->type->ptr_to));
    else if (stmt->rhs->type->kind == PTR || stmt->rhs->type->kind == ARRAY)
      printf("  imul rax,%d\n", type_size(stmt->rhs->type->ptr_to));
    printf("  add rax,rdi\n");
    break;
  case SUB:
    if (stmt->lhs->type->kind == PTR && is_integer(stmt->rhs->type))
      printf("  imul rdi, %d\n", type_size(stmt->lhs->type->ptr_to));
    printf("  sub rax,rdi\n");
    if (stmt->lhs->type->kind == PTR && stmt->rhs->type->kind == PTR) {
      printf("  mov rdi, %d\n", type_size(stmt->lhs->type->ptr_to));
      printf("  cqo\n");
      printf("  idiv rdi\n");
    }
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
  else if (type_size(type) == 1)
    printf("movsx rax, BYTE PTR [rax]\n");
  else
    not_implemented(__func__);
}

void store2rdiaddr_from_rax(Type *type) {
  if (type_size(type) == 8)
    printf("  mov [rdi],rax\n");
  else if (type_size(type) == 4)
    printf("  mov [rdi],eax\n");
  else if (type_size(type) == 1)
    printf("  mov [rdi],al\n");
  else
    not_implemented(__func__);
}
