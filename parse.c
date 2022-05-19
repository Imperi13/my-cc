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

Function *functions;
Function *now_function;

LVar *find_lvar(Token *tok){
  for(LVar *var = now_function->locals; var; var = var->next)
    if (var->len == tok->len && !memcmp(tok->str,var->name,var->len))
      return var;
  return NULL;
}

Function *find_function(Token *tok) {
  for(Function *func = functions;func;func = func->next)
    if(func->func_name_len == tok->len && !memcmp(tok->str,func->func_name,func->func_name_len))
      return func;
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
  while(!at_eof()){
    Function *push_function = func_definition();
    if(!functions) {
      functions = push_function;
      continue;
    }
    push_function -> next = functions;
    functions = push_function;
  }
}

Function *func_definition() {
  Function *func_def = calloc(1,sizeof(Function));
  now_function = func_def;

  Token *tok = consume_kind(TK_IDENT);
  if(!tok)
    error("invalid func definition");

  Function *double_define = find_function(tok);
  if(double_define)
    error("already defined");

  func_def->func_name = tok->str;
  func_def->func_name_len = tok->len;
  func_def->arg_count = 0;

  expect("(");
  if(!consume(")")){
    do{
      tok = consume_kind(TK_IDENT);
      LVar *lvar = find_lvar(tok);
      if(lvar)
        error("duplicate arguments");
      lvar = calloc(1,sizeof(LVar));
      lvar->next = now_function->locals;
      lvar->name = tok->str;
      lvar->len = tok->len;
      if(now_function->locals)
        lvar->offset = now_function->locals->offset + 8;
      else
        lvar->offset = 8;
      now_function->locals = lvar;

      func_def->arg_count++;

      if(func_def->arg_count > 6)
        error("more than 6 args is not implemented");
    }while(consume(","));
    expect(")");
  }

  expect("{");

  while(!consume("}")){
    StmtList *push_stmt = calloc(1,sizeof(StmtList));
    push_stmt->stmt = stmt();
    if(!func_def->code_front){
      func_def->code_front = push_stmt;
      func_def->code_back = push_stmt;
      continue;
    }
    func_def->code_back->next = push_stmt;
    func_def->code_back = push_stmt;
  }

  return func_def;
}

Node *stmt() {
  Node *node;

  if(consume("{")){
    node = calloc(1,sizeof(Node));
    node->kind = ND_BLOCK;
    while(!consume("}")){
      StmtList *push_stmt=calloc(1,sizeof(StmtList));
      push_stmt->stmt = stmt();
      if(!node->stmt_back){
        node->stmt_front=push_stmt;
        node->stmt_back=push_stmt;
        continue;
      }
      node->stmt_back->next = push_stmt;
      node->stmt_back = push_stmt;
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
  if(consume("*"))
    return new_node(ND_DEREF,unary(),NULL);
  if(consume("&"))
    return new_node(ND_ADDR,unary(),NULL);
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
      
      while(!consume(")")){
        ExprList *push_expr = calloc(1,sizeof(ExprList));
        push_expr->expr = expr();

        if(!node->expr_back){
          node->expr_front = push_expr;
          node->expr_back = push_expr;
        }else{
          node->expr_back->next = push_expr;
          node->expr_back = push_expr;
        }

        consume(",");
      }

      return node;
    }

    node->kind = ND_LVAR;

    LVar *lvar = find_lvar(tok);
    if(lvar) {
      node->offset=lvar->offset;
    } else {
      lvar = calloc(1,sizeof(LVar));
      lvar->next = now_function->locals;
      lvar->name = tok->str;
      lvar->len = tok->len;
      if(now_function->locals)
        lvar->offset = now_function->locals->offset + 8;
      else
        lvar->offset = 8;
      node->offset = lvar->offset;
      now_function->locals = lvar;
    }
    return node;
  }

  return new_node_num(expect_number());
}
