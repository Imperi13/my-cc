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

Global *functions;
Global *now_function;

LVar *find_lvar(Token *tok){
  for(LVarList *var = now_function->locals; var; var = var->next)
    if (var->lvar->len == tok->len && !memcmp(tok->str,var->lvar->name,var->lvar->len))
      return var->lvar;
  return NULL;
}

Global *find_function(Token *tok) {
  for(Global *func = functions;func;func = func->next)
    if(func->len == tok->len && !memcmp(tok->str,func->name,func->len))
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

Node *new_assign_node(Node *lhs,Node *rhs) {
  if(!is_convertible(lhs->type,rhs->type))
    error("invalid argument type to assign =");
  if(lhs->kind != ND_LVAR && lhs->kind != ND_DEREF)
    error("lhs is not lvalue");
  if(lhs->type->ty == ARRAY)
    error("cannot assign to ARRAY");
  Node *node = new_node(ND_ASSIGN,lhs,rhs,rhs->type);
  return node;
}

Type *parse_type(Token **rest,Token *tok) {
  if(!consume_kind(&tok,tok,TK_INT)){
    *rest = tok;
    return NULL;
  }
  Type *type = calloc(1,sizeof(Type));
  type->ty = INT;
  while(consume(&tok,tok,"*")){
    Type *now = calloc(1,sizeof(Type));
    now->ty = PTR;
    now->ptr_to = type;
    type = now;
  }
  *rest = tok;
  return type;
}

LVar *parse_lvar_definition(Token **rest,Token *tok) {
  Type *base_type = parse_type(&tok,tok);
  if(!base_type){
    *rest = tok;
    return NULL;
  }
  Token *ident = consume_kind(&tok,tok,TK_IDENT);
  if(!ident)
    error("error at var_def");

  if(consume(&tok,tok,"[")){
    Type *var_type = calloc(1,sizeof(Type));
    var_type->ty = ARRAY;
    var_type->ptr_to = base_type;
    var_type->array_size = expect_number(&tok,tok);
    expect(&tok,tok,"]");

    base_type = var_type;
  }
  
  LVar *lvar = find_lvar(ident);
  if(lvar)
    error("duplicate local variables");

  lvar = calloc(1,sizeof(LVar));
  lvar->name = ident->str;
  lvar->len = ident->len;
  lvar->type = base_type;

  *rest = tok;
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

void program(Token *tok) {
  while(!at_eof(tok)){
    Global *push_function = func_definition(&tok,tok);
    if(!functions) {
      functions = push_function;
      continue;
    }
    push_function -> next = functions;
    functions = push_function;
  }
}

Global *func_definition(Token **rest,Token *tok) {
  Type *return_type = parse_type(&tok,tok);
  if(!return_type)
    error("not type");

  Global *func_def = calloc(1,sizeof(Global));
  now_function = func_def;

  Token *ident = consume_kind(&tok,tok,TK_IDENT);
  if(!ident)
    error("invalid func definition");

  Global *double_define = find_function(ident);
  if(double_define)
    error("already defined");

  Type *type = calloc(1,sizeof(Type));
  type->ty = FUNC;
  type->return_type = return_type;

  func_def->type = type;
  func_def->name = ident->str;
  func_def->len = ident->len;
  func_def->arg_size = 0;

  expect(&tok,tok,"(");
  if(!consume(&tok,tok,")")){
    do{
      Type *arg_type = parse_type(&tok,tok);
      if(!arg_type)
        error("not type");

      TypeList *push_argtype = calloc(1,sizeof(TypeList));
      push_argtype->type = arg_type;

      if(!type->argtype_front){
        type->argtype_front = push_argtype;
        type->argtype_back = push_argtype;
      }else{
        type->argtype_back->next = push_argtype;
        type->argtype_back = push_argtype;
      }

      ident = consume_kind(&tok,tok,TK_IDENT);
      LVar *lvar = find_lvar(ident);
      if(lvar)
        error("duplicate arguments");

      LVarList *push_lvar = calloc(1,sizeof(LVarList));
      lvar = calloc(1,sizeof(LVar));
      push_lvar->lvar = lvar;
      push_lvar->next = now_function->locals;
      lvar->name = ident->str;
      lvar->len = ident->len;
      lvar->type = arg_type;
      if(now_function->locals)
        lvar->offset = offset_alignment(now_function->locals->lvar->offset,type_size(arg_type),type_alignment(arg_type));
      else
        lvar->offset = offset_alignment(0,type_size(arg_type),type_alignment(arg_type));
      now_function->locals = push_lvar;

      func_def->arg_size++;
      LVarList *push_arg = calloc(1,sizeof(LVarList));
      push_arg->lvar = lvar;
      if(!func_def->arg_front){
        func_def->arg_front = push_arg;
        func_def->arg_back = push_arg;
      }else{
        func_def->arg_back->next = push_arg;
        func_def->arg_back = push_arg;
      }

      if(func_def->arg_size > 6)
        error("more than 6 args is not implemented");
    }while(consume(&tok,tok,","));
    expect(&tok,tok,")");
  }

  expect(&tok,tok,"{");

  while(!consume(&tok,tok,"}")){
    NodeList *push_stmt = calloc(1,sizeof(NodeList));
    push_stmt->node = stmt(&tok,tok);
    if(!func_def->code_front){
      func_def->code_front = push_stmt;
      func_def->code_back = push_stmt;
    }else{
      func_def->code_back->next = push_stmt;
      func_def->code_back = push_stmt;
    }
  }

  *rest = tok;
  return func_def;
}

Node *stmt(Token **rest,Token *tok) {
  Node *node;

  if(consume(&tok,tok,"{")){
    node = calloc(1,sizeof(Node));
    node->kind = ND_BLOCK;
    while(!consume(&tok,tok,"}")){
      NodeList *push_stmt=calloc(1,sizeof(NodeList));
      push_stmt->node = stmt(&tok,tok);
      if(!node->stmt_back){
        node->stmt_front=push_stmt;
        node->stmt_back=push_stmt;
        continue;
      }
      node->stmt_back->next = push_stmt;
      node->stmt_back = push_stmt;
    }

    *rest = tok;
    return node;
  }

  if(consume_kind(&tok,tok,TK_IF)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_IF;
    expect(&tok,tok,"(");
    node -> expr = expr(&tok,tok);
    expect(&tok,tok,")");
    node -> lhs = stmt(&tok,tok);

    if(consume_kind(&tok,tok,TK_ELSE)){
      node -> kind = ND_IFELSE;
      node -> rhs = stmt(&tok,tok);
    }

    *rest = tok;
    return node;
  }

  if(consume_kind(&tok,tok,TK_WHILE)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_WHILE;
    expect(&tok,tok,"(");
    node->expr = expr(&tok,tok);
    expect(&tok,tok,")");
    node->lhs = stmt(&tok,tok);

    *rest = tok;
    return node;
  }

  if(consume_kind(&tok,tok,TK_FOR)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_FOR;
    expect(&tok,tok,"(");
    if(!consume(&tok,tok,";")){
      node->init_expr = expr(&tok,tok);
      expect(&tok,tok,";");
    }
    if(!consume(&tok,tok,";")){
      node->expr=expr(&tok,tok);
      expect(&tok,tok,";");
    }else{
      node->expr=new_node_num(1);
    }
    if(!consume(&tok,tok,")")){
      node->update_expr=expr(&tok,tok);
      expect(&tok,tok,")");
    }

    node->lhs=stmt(&tok,tok);

    *rest = tok;
    return node;
  }

  LVar *lvar;

  if(consume_kind(&tok,tok,TK_RETURN)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_RETURN;
    node->lhs = expr(&tok,tok);
  }else if(lvar = parse_lvar_definition(&tok,tok),lvar){
    node = calloc(1,sizeof(Node));
    node->kind = ND_LVAR_DEFINE;

    LVarList *push_lvar = calloc(1,sizeof(LVarList));
    push_lvar->lvar = lvar;
    push_lvar->next = now_function->locals;
    if(now_function->locals)
      lvar->offset = offset_alignment(now_function->locals->lvar->offset,type_size(lvar->type),type_alignment(lvar->type));
    else 
      lvar->offset = offset_alignment(0,type_size(lvar->type),type_alignment(lvar->type));
    now_function->locals = push_lvar;
  }else{
    node = expr(&tok,tok);
  }
  expect(&tok,tok,";");

  *rest = tok;
  return node;
}

Node *expr(Token **rest,Token *tok) {
  Node *node = assign(&tok,tok);

  *rest = tok;
  return node;
}

Node *assign(Token **rest,Token *tok) {
  Node *lhs = equality(&tok,tok);
  if (consume(&tok,tok,"=")){
    Node *rhs = assign(&tok,tok);
    lhs = new_assign_node(lhs,rhs);
  }else if(consume(&tok,tok,"+=")){
    Node *rhs = assign(&tok,tok);
    Node *add_node = new_add_node(lhs,rhs);
    lhs = new_assign_node(lhs,add_node);
  }

  *rest = tok;
  return lhs;
}

Node *equality(Token **rest,Token *tok) {
  Node *lhs = relational(&tok,tok);
  for(;;) {
    if(consume(&tok,tok,"==")){
      Node *rhs = relational(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to equality ==");
      lhs = new_node(ND_EQUAL,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"!=")){
      Node *rhs = relational(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to equality !=");
      lhs = new_node(ND_NOT_EQUAL,lhs,rhs,lhs->type);
    }else{
      *rest = tok;
      return lhs;
    }
  }
}

Node *relational(Token **rest,Token *tok) {
  Node *lhs = add(&tok,tok);
  for(;;) {
    if(consume(&tok,tok,"<")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational <");
      lhs = new_node(ND_SMALLER,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"<=")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational <=");
      lhs = new_node(ND_SMALLER_EQUAL,lhs,rhs,lhs->type);
    }else if (consume(&tok,tok,">")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational >");
      lhs = new_node(ND_GREATER,lhs,rhs,lhs->type);
    }else if (consume(&tok,tok,">=")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error("invalid argument type to relational >=");
      lhs = new_node(ND_GREATER_EQUAL,lhs,rhs,lhs->type);
    }else{
      *rest = tok;
      return lhs;
    }
  }
}

Node *add(Token **rest,Token *tok) {
  Node *lhs = mul(&tok,tok);
  
  for(;;) {
    if(consume(&tok,tok,"+")){
      Node *rhs = mul(&tok,tok);
      lhs = new_add_node(lhs,rhs);
    }else if(consume(&tok,tok,"-")){
      Node *rhs = mul(&tok,tok);

      if(lhs->type->ty == INT && rhs->type->ty == INT)
        lhs = new_node(ND_SUB,lhs,rhs,lhs->type);
      else if(rhs->type->ty == INT){
        Type *convert_type = calloc(1,sizeof(Type));
        convert_type->ty = PTR;
        convert_type->ptr_to = lhs->type->ptr_to;
        lhs = new_node(ND_SUB,lhs,rhs,convert_type);
      }else
        error("invalid argument type to sub -");
    }else{
      *rest = tok;
      return lhs;
    }
  }
}

Node *mul(Token **rest,Token *tok) {
  Node *lhs = unary(&tok,tok);

  for(;;) {
    if(consume(&tok,tok,"*")){
      Node *rhs = unary(&tok,tok);
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error("invalid argument type to mul * ");
      lhs = new_node(ND_MUL,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"/")){
      Node *rhs = unary(&tok,tok);
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error("invalid argument type to div /");
      lhs = new_node(ND_DIV,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"%")){
      Node *rhs = unary(&tok,tok);
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error("invalid argument type to mod %");
      lhs = new_node(ND_MOD,lhs,rhs,lhs->type);
    }else{
      *rest = tok;
      return lhs;
    }
  }
}

Node *unary(Token **rest,Token *tok) {
  if(consume_kind(&tok,tok,TK_SIZEOF)){
    Node *node = unary(&tok,tok);
    node = new_node_num(type_size(node->type));

    *rest = tok;
    return node;
  }

  if(consume(&tok,tok,"+")){
    Node *node = unary(&tok,tok);
    *rest = tok;
    return node;
  }
  if(consume(&tok,tok,"-")){
    Node *node = unary(&tok,tok);
    if(node->type->ty != INT)
      error("invalid argument type ptr to unary");
    node =  new_node(ND_SUB,new_node_num(0), node,node->type);
    
    *rest = tok;
    return node;
  }
  if(consume(&tok,tok,"*")){
    Node *node = unary(&tok,tok);
    if(node->type->ty != PTR && node->type->ty != ARRAY)
      error("not dereference to type int");
    node = new_node(ND_DEREF,node,NULL,node->type->ptr_to);

    *rest = tok;
    return node;
  }
  if(consume(&tok,tok,"&")){
    Node *node = unary(&tok,tok);
    Type *type = calloc(1,sizeof(Type));
    type->ty = PTR;
    type->ptr_to = node->type;
    node = new_node(ND_ADDR,node,NULL,type);

    *rest = tok;
    return node;
  }
  
  Node *node = postfix(&tok,tok);

  *rest = tok;
  return node;
}

Node *postfix(Token **rest,Token *tok) {
  Node *lhs = primary(&tok,tok);

  for(;;){
    if(consume(&tok,tok,"[")){
      Node *rhs = expr(&tok,tok);
      Node *add_node = new_add_node(lhs,rhs);
      lhs = new_deref_node(add_node);
      expect(&tok,tok,"]");
    }else if(consume(&tok,tok,"++")){
      Node *add_node = new_add_node(lhs,new_node_num(1));
      lhs = new_assign_node(lhs,add_node);
    }else{
      *rest = tok;
      return lhs;
    }
  }
}

Node *primary(Token **rest,Token *tok) {
  if(consume(&tok,tok,"(")){
    Node *node = expr(&tok,tok);
    expect(&tok,tok,")");
    
    *rest = tok;
    return node;
  }

  Token *ident = consume_kind(&tok,tok,TK_IDENT);
  if(ident){
    Node *node = calloc(1,sizeof(Node));

    if(consume(&tok,tok,"(")){
      node->kind = ND_FUNCTION_CALL;

      // 関数の返り値は全部intということにしている
      Type *return_type = calloc(1,sizeof(Type));
      return_type->ty = INT;

      node->type = return_type;
      node->func_name = ident->str;
      node->func_name_len = ident->len;
      
      while(!consume(&tok,tok,")")){
        NodeList *push_expr = calloc(1,sizeof(NodeList));
        push_expr->node = expr(&tok,tok);

        if(!node->expr_back){
          node->expr_front = push_expr;
          node->expr_back = push_expr;
        }else{
          node->expr_back->next = push_expr;
          node->expr_back = push_expr;
        }

        consume(&tok,tok,",");
      }
  
      *rest = tok;
      return node;
    }

    node->kind = ND_LVAR;

    LVar *lvar = find_lvar(ident);
    if(!lvar)
      error("ident '%.*s' is not defined",ident->len,ident->str);
    node->offset=lvar->offset;
    node->type = lvar->type;

    *rest = tok;
    return node;
  }

  Node *node = new_node_num(expect_number(&tok,tok));

  *rest = tok;
  return node;
}
