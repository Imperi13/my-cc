#include "mycc.h"

void global_definition(Token **rest,Token *tok);
void var_definition(Token **rest,Token *tok);
void function_definition(Token **rest,Token *tok);
Node *stmt(Token **rest,Token *tok);
Node *expr(Token **rest,Token *tok);
Node *assign(Token **rest,Token *tok);
Node *equality(Token **rest,Token *tok);
Node *relational(Token **rest,Token *tok);
Node *add(Token **rest,Token *tok);
Node *mul(Token **rest,Token *tok);
Node *unary(Token **rest,Token *tok);
Node *postfix(Token **rest,Token *tok);
Node *primary(Token **rest,Token *tok);

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

ObjList *globals;
Obj *now_function;

Obj *find_obj(ObjList *list,char *str,int len){
  for(ObjList *var = list;var;var = var->next)
    if(var->obj->len == len && !memcmp(str,var->obj->name,var->obj->len))
      return var->obj;
  return NULL;
}

Obj *find_lvar(char *str,int len){
  for(ObjList *var = now_function->locals; var; var = var->next)
    if (var->obj->len == len && !memcmp(str,var->obj->name,var->obj->len))
      return var->obj;
  return NULL;
}

Obj *find_global(Token *tok) {
  for(ObjList *func = globals;func;func = func->next)
    if(func->obj->len == tok->len && !memcmp(tok->str,func->obj->name,func->obj->len))
      return func->obj;
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

  node->kind = ND_NUM;
  node->type = type_int;
  node->val = val;
  return node;
}

Node *new_add_node(Node *lhs,Node *rhs) {
  Node *node = calloc(1,sizeof(Node));
  node->kind = ND_ADD;
  node->lhs = lhs;
  node->rhs = rhs;

  if(is_numeric(lhs->type) && is_numeric(rhs->type))
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
  if(lhs->kind != ND_VAR && lhs->kind != ND_DEREF)
    error("lhs is not lvalue");
  if(lhs->type->ty == ARRAY)
    error("cannot assign to ARRAY");
  Node *node = new_node(ND_ASSIGN,lhs,rhs,rhs->type);
  return node;
}

int offset_alignment(int start,int data_size,int alignment){
  return ((start+data_size + alignment - 1)/alignment)*alignment;
}

void program(Token *tok) {
  while(!at_eof(tok)){
    global_definition(&tok,tok);
  }
}

void global_definition(Token **rest,Token *tok){
  Obj *tmp = parse_global_decl(&dummy_token,tok);
  if(!tmp)
    error_at(tok->str,"cannot parse global definition");
  if(tmp->type->ty == FUNC)
    function_definition(&tok,tok);
  else
    var_definition(&tok,tok);

  *rest = tok;
}

void var_definition(Token **rest,Token *tok) {
  ObjList *push_var = calloc(1,sizeof(ObjList));
  push_var->obj = parse_global_decl(&tok,tok);
  if(!globals) {
    globals = push_var;
  }else{
    push_var->next = globals;
    globals = push_var;
  }

  expect(&tok,tok,";");
  *rest = tok;
}

void function_definition(Token **rest,Token *tok) {
  ObjList *push_function = calloc(1,sizeof(ObjList));
  Obj *func_def = parse_global_decl(&tok,tok);
  push_function->obj = func_def;
  if(!globals) {
    globals = push_function;
  }else{
    push_function -> next = globals;
    globals = push_function;
  }

  now_function = func_def;

  if(!consume(&tok,tok,"{")){
    expect(&tok,tok,";");
    *rest = tok;
    return;
  }

  func_def->is_defined = true;

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

  if(consume_kind(&tok,tok,TK_RETURN)) {
    node = calloc(1,sizeof(Node));
    node->kind = ND_RETURN;
    node->lhs = expr(&tok,tok);
  }else if(parse_local_decl(&dummy_token,tok)){
    node = calloc(1,sizeof(Node));
    node->kind = ND_VAR_DEFINE;

    ObjList *push_lvar = calloc(1,sizeof(ObjList));
    Obj *lvar = parse_local_decl(&tok,tok);
    push_lvar->obj = lvar;
    push_lvar->next = now_function->locals;
    if(now_function->locals)
      lvar->offset = offset_alignment(now_function->locals->obj->offset,type_size(lvar->type),type_alignment(lvar->type));
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
        error_at(tok->str,"invalid argument type to equality ==");
      lhs = new_node(ND_EQUAL,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"!=")){
      Node *rhs = relational(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error_at(tok->str,"invalid argument type to equality !=");
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
        error_at(tok->str,"invalid argument type to relational <");
      lhs = new_node(ND_SMALLER,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"<=")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error_at(tok->str,"invalid argument type to relational <=");
      lhs = new_node(ND_SMALLER_EQUAL,lhs,rhs,lhs->type);
    }else if (consume(&tok,tok,">")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error_at(tok->str,"invalid argument type to relational >");
      lhs = new_node(ND_GREATER,lhs,rhs,lhs->type);
    }else if (consume(&tok,tok,">=")){
      Node *rhs = add(&tok,tok);
      if(!is_same_type(lhs->type,rhs->type))
        error_at(tok->str,"invalid argument type to relational >=");
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
        error_at(tok->str,"invalid argument type to sub -");
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
        error_at(tok->str,"invalid argument type to mul * ");
      lhs = new_node(ND_MUL,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"/")){
      Node *rhs = unary(&tok,tok);
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error_at(tok->str,"invalid argument type to div /");
      lhs = new_node(ND_DIV,lhs,rhs,lhs->type);
    }else if(consume(&tok,tok,"%")){
      Node *rhs = unary(&tok,tok);
      if(lhs->type->ty != INT || rhs->type->ty != INT)
        error_at(tok->str,"invalid argument type to mod %");
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
      error_at(tok->str,"invalid argument type ptr to unary");
    node =  new_node(ND_SUB,new_node_num(0), node,node->type);
    
    *rest = tok;
    return node;
  }
  if(consume(&tok,tok,"*")){
    Node *node = unary(&tok,tok);
    if(node->type->ty != PTR && node->type->ty != ARRAY)
      error_at(tok->str,"not dereference to type int");
    node = new_node(ND_DEREF,node,NULL,node->type->ptr_to);

    *rest = tok;
    return node;
  }
  if(consume(&tok,tok,"&")){
    Node *node = unary(&tok,tok);
    Type *type = newtype_ptr(node->type);
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
    }else if(consume(&tok,tok,"(")){
      Node *node = calloc(1,sizeof(Node));
      node->kind = ND_FUNCTION_CALL;
      node->lhs = lhs;

      // 関数の返り値は全部intということにしている
      node->type = type_int;
      
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
      lhs = node;
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
    node->kind = ND_VAR;

    Obj *var = find_lvar(ident->str,ident->len);
    if(var){
      node->offset = var->offset;
      node->type = var->type;

      *rest = tok;
      return node;
    }

    var = find_global(ident);
    if(var){
      node->type = var->type;
      node->name = var->name;
      node->len = var->len;
      node->is_defined = var->is_defined;
      node->is_global = true;

      *rest = tok;
      return node;
    }

    error_at(tok->str,"ident '%.*s' is not defined",ident->len,ident->str);
  }

  if(equal_kind(tok,TK_STR)){
    StrLiteral *str_literal = consume_kind(&tok,tok,TK_STR)->str_literal;

    Node *node = calloc(1,sizeof(Node));
    node->kind = ND_STR;
    node->str_literal = str_literal;
    node->type = newtype_ptr(type_char);

    *rest = tok;
    return node;
  }

  Node *node = new_node_num(expect_number(&tok,tok));

  *rest = tok;
  return node;
}
