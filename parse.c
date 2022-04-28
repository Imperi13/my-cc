#include "mycc.h"

bool consume(char* op){
  if (token->kind != TK_RESERVED || 
      strlen(op) != token->len ||
      memcmp(token->str,op,token->len))
    return false;
  token = token->next;
  return true;
}

void expect(char* op) {
  if (token->kind != TK_RESERVED || 
      strlen(op) != token->len ||
      memcmp(token->str,op,token->len))
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

Token *new_token(TokenKind kind,Token *cur,char *str,int len) {
  Token *tok = calloc(1,sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
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

   if(strncmp(p,">=",2) == 0 || strncmp(p,"<=",2) ==0 || strncmp(p,"==",2) == 0 || strncmp(p,"!=",2) == 0){
     cur = new_token(TK_RESERVED,cur,p,2);
     p+=2;
     continue;
   }

   if(*p == '<' || *p == '>' ){
     cur = new_token(TK_RESERVED,cur,p++,1);
     continue;
   }

   if(*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' || *p == ')') {
     cur = new_token(TK_RESERVED,cur,p++,1);
     continue;
   }

   if (isdigit(*p)) {
     char *prev = p;
     cur = new_token(TK_NUM,cur,p,1);
     cur->val = strtol(p,&p,10);
     cur->len = p-prev;
     continue;
   }

   error("cannot tokenize");
 }

 new_token(TK_EOF,cur,p,0);
 return head.next;
}

void debug_token() {
  Token *cur = token;
  while(cur != NULL){
    fprintf(stderr,"kind:%d , len :%d , str: %.*s\n",cur->kind,cur->len,cur->len,cur->str);
    cur = cur->next;
  }
}
