#include "error.h"
#include "file.h"
#include "tokenize.h"
#include "parse.h"

char *filename;
char *user_input;

int main(int argc, char **argv) {
  if (argc != 2)
    error("invalid argv");

  filename = argv[1];
  user_input = read_file(argv[1]);
  Token *token = tokenize(user_input);

  // debug_token(token);

  Tree *ast = parse_translation_unit(token);
  
  // codegen_all(stdout);
  return 0;
}
