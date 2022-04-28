#include "mycc.h"

void error(char *fmt, ...) {
  va_list ap;
  va_start(ap,fmt);
  vfprintf(stderr,fmt,ap);
  fprintf(stderr,"\n");
  exit(1);
}

Token *token;
char *user_input;

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

int main(int argc,char **argv){
  if(argc != 2){
    fprintf(stderr, "invalid argv");
    return 1;
  }

  user_input = argv[1];
  token = tokenize(user_input);

  //debug_token();

  Node *node = expr();

  printf(".intel_syntax noprefix\n");
  printf(".globl main\n");
  printf("main:\n");
  
  gen(node);

  printf("  pop rax\n");
  printf("  ret\n");
  return 0;
}
