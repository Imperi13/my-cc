#include "mycc.h"

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

char *filename;
char *user_input;

int main(int argc,char **argv){
  if(argc != 2){
    fprintf(stderr, "invalid argv");
    return 1;
  }

  filename = argv[1];
  user_input = read_file(argv[1]);
  Token *token = tokenize(user_input);

  //debug_token(token);

  program(token);
  
  codegen_all(stdout);
  return 0;
}
