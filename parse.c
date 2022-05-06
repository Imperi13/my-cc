#include "mycc.h"

bool consume(char* op){
  if (token->kind != TK_RESERVED || 
      strlen(op) != token->len ||
      memcmp(token->str,op,token->len))
    return false;
  token = token->next;
  return true;
}

Token *consume_kind(TokenKind kind) {
  if (token->kind != kind)
    return NULL;
  Token *tok = token;
  token = token->next;
  return tok;
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

const char variable_letters[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

bool is_alnum(char c) {
  for(const char *p = variable_letters;*p!='\0';p++)
    if(*p == c)
      return true;
  return false;
}

Token *tokenize(char *p){
 Token head;
 head.next = NULL;
 Token *cur = &head;

 while(*p) {
   if (isspace(*p) || *p == '\n') {
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

   if(*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' || *p == ')' || *p == '=' || *p == ';') {
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

   if(strncmp(p,"return",6) == 0 && !is_alnum(p[6])) {
     cur = new_token(TK_RETURN,cur,p,6);
     p+=6;
     continue;
   }

   if(strncmp(p,"if",2) == 0 && !is_alnum(p[2])) {
     cur = new_token(TK_IF,cur,p,2);
     p+=2;
     continue;
   }

   if(strncmp(p,"else",4) == 0 && !is_alnum(p[4])) {
     cur = new_token(TK_ELSE,cur,p,4);
     p+=4;
     continue;
   }

   if(strncmp(p,"while",5) == 0 && !is_alnum(p[5])) {
     cur = new_token(TK_WHILE,cur,p,5);
     p+=5;
     continue;
   }

   if(strncmp(p,"for",3) == 0 && !is_alnum(p[3])) {
     cur = new_token(TK_FOR,cur,p,3);
     p += 3;
     continue;
   }

   if (strspn(p,variable_letters) > 0){
     int len = strspn(p,variable_letters);
     cur = new_token(TK_IDENT,cur,p,len);
     p+=len;
     continue;
   }

   error("cannot tokenize");
 }

 new_token(TK_EOF,cur,p,0);
 return head.next;
}

LVar *locals;

LVar *find_lvar(Token *tok){
  for(LVar *var = locals; var; var = var->next)
    if (var->len == tok->len && !memcmp(tok->str,var->name,var->len))
      return var;
  return NULL;
}

void debug_token() {
  Token *cur = token;
  while(cur != NULL){
    fprintf(stderr,"kind:%d , len :%d , str: %.*s\n",cur->kind,cur->len,cur->len,cur->str);
    cur = cur->next;
  }
}
