#include "mycc.h"

void error(char *fmt, ...) {
  va_list ap;
  va_start(ap,fmt);
  vfprintf(stderr,fmt,ap);
  fprintf(stderr,"\n");
  exit(1);
}

void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap,fmt);

  int pos = loc - user_input;
  fprintf(stderr, "%s\n", user_input);
  fprintf(stderr,"%*s",pos, " ");
  fprintf(stderr,"^ ");
  vfprintf(stderr,fmt,ap);
  fprintf(stderr, "\n");
  exit(1);
}

LVar *locals;

LVar *find_lvar(Token *tok){
  for(LVar *var = locals; var; var = var->next)
    if (var->len == tok->len && !memcmp(tok->str,var->name,var->len))
      return var;
  return NULL;
}

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
  while(!at_eof()){
    StmtList *push_stmt = calloc(1,sizeof(StmtList));
    push_stmt -> stmt = stmt();
    if(!code_back) {
      code_front = push_stmt;
      code_back = push_stmt;
      continue;
    }
    code_back->next = push_stmt;
    code_back = push_stmt;
  }
}   

Node *stmt() {
  Node *node;

  if(consume("{")){
    node = calloc(1,sizeof(Node));
    node->kind = ND_BLOCK;
    while(!consume("}")){
      StmtList *push_stmt=calloc(1,sizeof(StmtList));
      push_stmt->stmt = stmt();
      if(!node->back){
        node->front=push_stmt;
        node->back=push_stmt;
        continue;
      }
      node->back->next = push_stmt;
      node->back = push_stmt;
    }
    return node;
  }

  if(consume_kind(TK_IF)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_IF;
    expect("(");
    node -> expr = expr();
    expect(")");
    node -> lhs = stmt();

    if(consume_kind(TK_ELSE)){
      node -> kind = ND_IFELSE;
      node -> rhs = stmt();
    }
    return node;
  }

  if(consume_kind(TK_WHILE)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_WHILE;
    expect("(");
    node->expr = expr();
    expect(")");
    node->lhs = stmt();
    return node;
  }

  if(consume_kind(TK_FOR)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_FOR;
    expect("(");
    if(!consume(";")){
      node->init_expr = expr();
      expect(";");
    }
    if(!consume(";")){
      node->expr=expr();
      expect(";");
    }else{
      node->expr=new_node_num(1);
    }
    if(!consume(")")){
      node->update_expr=expr();
      expect(")");
    }

    node->lhs=stmt();
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
    if(consume("(")){
      node->kind = ND_FUNCTION_CALL;
      node->func_name = tok->str;
      node->func_name_len = tok->len;
      expect(")");
      return node;
    }
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
