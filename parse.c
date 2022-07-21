#include <stdio.h>
#include <stdlib.h>

#include "parse.h"
#include "error.h"
#include "tokenize.h"

static Tree *parse_translation_unit(Token *tok);
static Tree *parse_external_decl(Token **rest,Token *tok);

Tree *parse_translation_unit(Token *tok){
  Tree *head = calloc(1,sizeof(Tree));
  Tree *cur = head;

  while(!at_eof(tok)){
    Tree *ex_decl = parse_external_decl(&tok,tok);
    cur->next = ex_decl;
    cur = ex_decl;
  }
  return head->next;
}

Tree *parse_external_decl(Token **rest,Token *tok){
  not_implemented();
  return NULL;
}
