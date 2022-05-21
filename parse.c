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

Node *new_node(NodeKind kind, Node *lhs, Node *rhs,Type *type) {
  Node *node = calloc(1,sizeof(Node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  node->type = type;
  return node;
}

Node *new_node_num(int val) {
  Node *node = calloc(1,sizeof(Node));
  Type *type = calloc(1,sizeof(Type));
  type->ty = INT;

  node->kind = ND_NUM;
  node->type = type;
  node->val = val;
  return node;
}

Node *new_add_node(Node *lhs,Node *rhs) {
  Node *node = calloc(1,sizeof(Node));
  node->kind = ND_ADD;
  node->lhs = lhs;
  node->rhs = rhs;

  if(lhs->type->ty == INT && rhs->type->ty == INT)
    node->type = lhs->type;
  else if(lhs->type->ty == INT){
    node->type = calloc(1,sizeof(Type));
    node->type->ty = PTR;
    node->type->ptr_to = rhs->type->ptr_to;
  }else if(rhs->type->ty == INT){
    node->type = calloc(1,sizeof(Type));
    node->type->ty = PTR;
    node->type->ptr_to = lhs->type->ptr_to;
  }else
    error("invalid argument type to add +");

  return node;
}

Node *new_deref_node(Node *lhs) {
  Node *node = calloc(1,sizeof(Node));
  node->kind = ND_DEREF;
  node->lhs = lhs;

  if(lhs->type->ty != PTR && lhs->type->ty != ARRAY)
    error("not dereference to type int");
  node->type = lhs->type->ptr_to;
  return node;
}

Type *parse_type() {
  if(!consume_kind(TK_INT))
    return NULL;
  Type *type = calloc(1,sizeof(Type));
  type->ty = INT;
  while(consume("*")){
    Type *now = calloc(1,sizeof(Type));
    now->ty = PTR;
    now->ptr_to = type;
    type = now;
  }
  return type;
}

LVar *parse_lvar_definition() {
  Type *base_type = parse_type();
  if(!base_type)
    return NULL;
  Token *tok = consume_kind(TK_IDENT);
  if(!tok)
    error("error at var_def");

  if(consume("[")){
    Type *var_type = calloc(1,sizeof(Type));
    var_type->ty = ARRAY;
    var_type->ptr_to = base_type;
    var_type->array_size = expect_number();
    expect("]");

    base_type = var_type;
  }
  
  LVar *lvar = find_lvar(tok);
  if(lvar)
    error("duplicate local variables");

  lvar = calloc(1,sizeof(LVar));
  lvar->name = tok->str;
  lvar->len = tok->len;
  lvar->type = base_type;
  return lvar;
}

bool is_same_type(Type *a,Type *b){
  if(a->ty == INT && b->ty == INT)
    return true;
  if(a->ty != b->ty)
    return false;
  return is_same_type(a->ptr_to,b->ptr_to);
}

bool is_convertible(Type *a,Type *b){
  if(a->ty == INT && b->ty == INT)
    return true;
  if(a->ty == INT || b->ty == INT)
    return false;
  return is_same_type(a->ptr_to,b->ptr_to);
}

int type_size(Type *a){
  if(a->ty == INT)
    return 4;
  if(a->ty == ARRAY)
    return a->array_size * type_size(a->ptr_to);
  return 8;
}

int type_alignment(Type *a){
  if(a->ty == INT)
    return 4;
  if(a->ty == ARRAY)
    return type_alignment(a->ptr_to);
  return 8;
}

int offset_alignment(int start,int data_size,int alignment){
  return ((start+data_size + alignment - 1)/alignment)*alignment;
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
  Type *return_type = parse_type();
  if(!return_type)
    error("not type");

  Function *func_def = calloc(1,sizeof(Function));
  now_function = func_def;

  Token *tok = consume_kind(TK_IDENT);
  if(!tok)
    error("invalid func definition");

  Function *double_define = find_function(tok);
  if(double_define)
    error("already defined");

  func_def->return_type = return_type;
  func_def->func_name = tok->str;
  func_def->func_name_len = tok->len;
  func_def->arg_count = 0;

  expect("(");
  if(!consume(")")){
    do{
      Type *arg_type = parse_type();
      if(!arg_type)
        error("not type");

      tok = consume_kind(TK_IDENT);
      LVar *lvar = find_lvar(tok);
      if(lvar)
        error("duplicate arguments");

      lvar = calloc(1,sizeof(LVar));
      lvar->next = now_function->locals;
      lvar->name = tok->str;
      lvar->len = tok->len;
      lvar->type = arg_type;
      if(now_function->locals)
        lvar->offset = offset_alignment(now_function->locals->offset,type_size(arg_type),type_alignment(arg_type));
      else
        lvar->offset = offset_alignment(0,type_size(arg_type),type_alignment(arg_type));
      now_function->locals = lvar;

      func_def->arg_count++;
      ArgList *push_type = calloc(1,sizeof(ArgList));
      push_type->type = arg_type;
      push_type->lvar = lvar;
      if(!func_def->arg_front){
        func_def->arg_front = push_type;
        func_def->arg_back = push_type;
      }else{
        func_def->arg_back->next = push_type;
        func_def->arg_back = push_type;
      }

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
    }else{
      func_def->code_back->next = push_stmt;
      func_def->code_back = push_stmt;
    }
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

  LVar *lvar;

  if(consume_kind(TK_RETURN)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_RETURN;
    node->lhs = expr();
  }else if(lvar = parse_lvar_definition(),lvar){
    node = calloc(1,sizeof(Node));
    node->kind = ND_LVAR_DEFINE;

    lvar->next = now_function->locals;
    if(now_function->locals)
      lvar->offset = offset_alignment(now_function->locals->offset,type_size(lvar->type),type_alignment(lvar->type));
    else 
      lvar->offset = offset_alignment(0,type_size(lvar->type),type_alignment(lvar->type));
    now_function->locals = lvar;
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
  Node *lhs = equality();
  if (consume("=")){
    Node *rhs = assign();
    if(!is_convertible(lhs->type,rhs->type))
      error("invalid argument type to assign =");
    if(lhs->type->ty == ARRAY)
      error("not assignable ARRAY");
    lhs = new_node(ND_ASSIGN,lhs,rhs,rhs->type);
  }
  return lhs;
}

Node *equality() {
  Node *lhs = relational();
  for(;;) {
    if(consume("==")){
      Node *rhs = relational();
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to equality ==");
      lhs = new_node(ND_EQUAL,lhs,rhs,lhs->type);
    }else if(consume("!=")){
      Node *rhs = relational();
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to equality !=");
      lhs = new_node(ND_NOT_EQUAL,lhs,rhs,lhs->type);
    }else
      return lhs;
  }
}

Node *relational() {
  Node *lhs = add();
  for(;;) {
    if(consume("<")){
      Node *rhs = add();
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational <");
      lhs = new_node(ND_SMALLER,lhs,rhs,lhs->type);
    }else if(consume("<=")){
      Node *rhs = add();
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational <=");
      lhs = new_node(ND_SMALLER_EQUAL,lhs,rhs,lhs->type);
    }else if (consume(">")){
      Node *rhs = add();
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational >");
      lhs = new_node(ND_GREATER,lhs,rhs,lhs->type);
    }else if (consume(">=")){
      Node *rhs = add();
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational >=");
      lhs = new_node(ND_GREATER_EQUAL,lhs,rhs,lhs->type);
    }else
      return lhs;
  }
}

Node *add() {
  Node *lhs = mul();
  
  for(;;) {
    if(consume("+")){
      Node *rhs = mul();
      lhs = new_add_node(lhs,rhs);
    }else if(consume("-")){
      Node *rhs = mul();

      if(lhs->type->ty == INT && rhs->type->ty == INT)
        lhs = new_node(ND_SUB,lhs,rhs,lhs->type);
      else if(rhs->type->ty == INT){
        Type *convert_type = calloc(1,sizeof(Type));
        convert_type->ty = PTR;
        convert_type->ptr_to = lhs->type->ptr_to;
        lhs = new_node(ND_SUB,lhs,rhs,convert_type);
      }else
        error("invalid argument type to sub -");
    }else
      return lhs;
  }
}

Node *mul() {
  Node *lhs = unary();

  for(;;) {
    if(consume("*")){
      Node *rhs = unary();
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error("invalid argument type to mul * ");
      lhs = new_node(ND_MUL,lhs,rhs,lhs->type);
    }else if(consume("/")){
      Node *rhs = unary();
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error("invali argument type to div /");
      lhs = new_node(ND_DIV,lhs,rhs,lhs->type);
    }else 
      return lhs;
  }
}

Node *unary() {
  if(consume_kind(TK_SIZEOF)){
    Node *node = unary();
    return new_node_num(type_size(node->type));
  }

  if(consume("+"))
    return unary();
  if(consume("-")){
    Node *node = unary();
    if(node->type->ty != INT)
      error("invalid argument type ptr to unary");
    return new_node(ND_SUB,new_node_num(0), node,node->type);
  }
  if(consume("*")){
    Node *node = unary();
    if(node->type->ty != PTR && node->type->ty != ARRAY)
      error("not dereference to type int");
    return new_node(ND_DEREF,node,NULL,node->type->ptr_to);
  }
  if(consume("&")){
    Node *node = unary();
    Type *type = calloc(1,sizeof(Type));
    type->ty = PTR;
    type->ptr_to = node->type;
    return new_node(ND_ADDR,node,NULL,type);
  }
  return postfix();
}

Node *postfix() {
  Node *lhs = primary();

  for(;;){
    if(consume("[")){
      Node *rhs = expr();
      Node *add_node = new_add_node(lhs,rhs);
      lhs = new_deref_node(add_node);
      expect("]");
    }else
      return lhs;
  }
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

      // 関数の返り値は全部intということにしている
      Type *return_type = calloc(1,sizeof(Type));
      return_type->ty = INT;

      node->type = return_type;
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
    if(!lvar)
      error("ident '%.*s' is not defined",tok->len,tok->str);
    node->offset=lvar->offset;
    node->type = lvar->type;
    return node;
  }

  return new_node_num(expect_number());
}
