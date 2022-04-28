#include "mycc.h"

bool consume(char op){
  if (token->kind != TK_RESERVED || token->str[0] != op)
    return false;
  token = token->next;
  return true;
}

void expect(char op) {
  if (token->kind != TK_RESERVED || token->str[0] != op)
    error("not '%c' op",op);
  token = token->next;
}

int expect_number(){
  if (token->kind != TK_NUM)
    error("not number");
  int val = token->val;
  token = token->next;
  return val;
}

bool at_eof(){
  return token->kind == TK_EOF;
}

Token *new_token(TokenKind kind,Token *cur,char *str) {
  Token *tok = calloc(1,sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  cur->next = tok;
  return tok;
}

Token *tokenize(char *p){
 Token head;
 head.next = NULL;
 Token *cur = &head;

 while(*p) {
   if (isspace(*p)) {
     p++;
     continue;
   }

   if(*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' || *p == ')') {
     cur = new_token(TK_RESERVED,cur,p++);
     continue;
   }

   if (isdigit(*p)) {
     cur = new_token(TK_NUM,cur,p);
     cur->val = strtol(p,&p,10);
     continue;
   }

   error("cannot tokenize");
 }

 new_token(TK_EOF,cur,p);
 return head.next;
}
