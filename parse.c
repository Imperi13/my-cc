#include <stdio.h>
#include <stdlib.h>

#include "parse.h"
#include "tokenize.h"

Tree *parse_translation_unit(Token *tok){
  Tree *head = calloc(1,sizeof(Tree));

  while(!at_eof(tok)){
  }
  return head->next;
}
