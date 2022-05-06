#include "mycc.h"

Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
  Node *node = calloc(1,sizeof(Node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

Node *new_node_num(int val) {
  Node *node = calloc(1,sizeof(Node));
  node->kind = ND_NUM;
  node->val = val;
  return node;
}

void program() {
  int i = 0;
  while(!at_eof())
    code[i++] = stmt();
  code[i] = NULL;
}

Node *stmt() {
  Node *node;

  if(consume_kind(TK_IF)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_IF;
    expect("(");
    node -> expr = expr();
    expect(")");
    node -> lhs = stmt();
    return node;
  }

  if(consume_kind(TK_RETURN)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_RETURN;
    node->lhs = expr();
  }else{
    node = expr();
  }
  expect(";");
  return node;
}

Node *expr() {
  return assign();
}

Node *assign() {
  Node *node = equality();
  if (consume("="))
    node = new_node(ND_ASSIGN,node,assign());
  return node;
}

Node *equality() {
  Node *node = relational();
  for(;;) {
    if(consume("=="))
      node = new_node(ND_EQUAL,node,relational());
    else if(consume("!="))
      node = new_node(ND_NOT_EQUAL,node,relational());
    else
      return node;
  }
}

Node *relational() {
  Node *node = add();
  for(;;) {
    if(consume("<"))
      node = new_node(ND_SMALLER,node,add());
    else if(consume("<="))
      node = new_node(ND_SMALLER_EQUAL,node,add());
    else if (consume(">"))
      node = new_node(ND_GREATER,node,add());
    else if (consume(">="))
      node = new_node(ND_GREATER_EQUAL,node,add());
    else
      return node;
  }
}

Node *add() {
  Node *node = mul();
  
  for(;;) {
    if(consume("+"))
      node = new_node(ND_ADD, node,mul());
    else if(consume("-"))
      node = new_node(ND_SUB,node,mul());
    else
      return node;
  }
}

Node *mul() {
  Node *node = unary();

  for(;;) {
    if(consume("*"))
      node = new_node(ND_MUL,node,unary());
    else if(consume("/"))
      node = new_node(ND_DIV,node,unary());
    else 
      return node;
  }
}

Node *unary() {
  if(consume("+"))
    return primary();
  if(consume("-"))
    return new_node(ND_SUB,new_node_num(0), primary());
  return primary();
}

Node *primary() {
  if(consume("(")){
    Node *node = expr();
    expect(")");
    return node;
  }

  Token *tok = consume_kind(TK_IDENT);
  if(tok){
    Node *node = calloc(1,sizeof(Node));
    node->kind = ND_LVAR;

    LVar *lvar = find_lvar(tok);
    if(lvar) {
      node->offset=lvar->offset;
    } else {
      lvar = calloc(1,sizeof(LVar));
      lvar->next = locals;
      lvar->name = tok->str;
      lvar->len = tok->len;
      if(locals)
        lvar->offset = locals->offset + 8;
      else
        lvar->offset = 8;
      node->offset = lvar->offset;
      locals = lvar;
    }
    return node;
  }

  return new_node_num(expect_number());
}

int label_count = 0;

void gen_lval(Node *node) {
  if (node->kind != ND_LVAR)
    error("not lval");

  printf("  mov rax, rbp\n");
  printf("  sub rax, %d\n",node->offset);
  printf("  push rax\n");
}

void gen(Node *node) {
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
      gen(node->expr);
      printf("  pop rax\n");
      printf("  cmp rax,0\n");
      printf("  je .Lend%d\n",label_count);
      gen(node->lhs);
      printf(".Lend%d:\n",label_count);
      label_count++;
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
