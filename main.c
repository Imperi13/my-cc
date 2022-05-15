#include "mycc.h"

void error(char *fmt, ...) {
  va_list ap;
  va_start(ap,fmt);
  vfprintf(stderr,fmt,ap);
  fprintf(stderr,"\n");
  exit(1);
}

char *read_file(char *path) {
  FILE *fp = fopen(path,"r");
  if(!fp)
    error("cannot open %s: %s",path,strerror(errno));

  if(fseek(fp,0,SEEK_END) == -1)
    error("%s: fseek: %s",path,strerror(errno));
  size_t size = ftell(fp);
  if(fseek(fp,0,SEEK_SET) == -1)
    error("%s: fseek: %s",path,strerror(errno));

  char *buf = calloc(1,size+2);
  fread(buf,size,1,fp);

  if(size==0 || buf[size-1] != '\n')
    buf[size++] = '\n';
  buf[size] = '\0';
  fclose(fp);
  return buf;
}

Token *token;
char *user_input;
Node *code[100];


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

  user_input = read_file(argv[1]);
  token = tokenize(user_input);

  //debug_token();

  program();
  
  codegen_all(stdout);
  return 0;
}
