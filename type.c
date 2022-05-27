#include "mycc.h"

Type *declaration_specifier(Token **rest,Token *tok);
Type *type_suffix(Token **rest,Token *tok,Type *type);
Type *declarator(Token **rest,Token *tok,Type *type);

Type *type_int = &(Type){.ty = INT};

Type *newtype_ptr(Type *base){
  Type *type = calloc(1,sizeof(Type));
  type->ty = PTR;
  type->ptr_to = base;
  return type;
}

Type *parse_decl(Token **rest,Token *tok){
  Type *type;
  Type *tmp;
  while(tmp = declaration_specifier(&tok,tok),tmp)
    type = tmp;

  type = declarator(&tok,tok,type);

  *rest = tok;
  return type;
}

Type *declaration_specifier(Token **rest,Token *tok){
  if(consume_kind(&tok,tok,TK_INT)){
    *rest = tok;
    return type_int;
  }
  *rest = tok;
  return NULL;
}

Type *type_suffix(Token **rest,Token *tok,Type *type){
  if(consume(&tok,tok,"(")){
    Type *func_type = calloc(1,sizeof(Type));
    func_type->ty = FUNC;
    func_type->return_type = type;

    if(!consume(&tok,tok,")")){
      do{
        Type *argtype = parse_decl(&tok,tok);

        TypeList *push_argtype = calloc(1,sizeof(TypeList));
        push_argtype->type = argtype;

        if(!type->argtype_back){
          type->argtype_front = push_argtype;
          type->argtype_back = push_argtype;
        }else{
          type->argtype_back->next = push_argtype;
          type->argtype_back = push_argtype;
        }

        type->arg_size++;
      }while(consume(&tok,tok,","));
      expect(&tok,tok,")");
    }

    *rest = tok;
    return func_type;
  }

  if(consume(&tok,tok,"[")){
    Type *array_type = calloc(1,sizeof(Type));
    array_type->ty = ARRAY;
    array_type->array_size = expect_number(&tok,tok);
    array_type->ptr_to = type;

    expect(&tok,tok,"]");

    *rest = tok;
    return array_type;
  }

  *rest = tok;
  return type;
}

Type *declarator(Token **rest,Token *tok,Type *type){
  while(consume(&tok,tok,"*")){
    type = newtype_ptr(type);
  }

  if(consume_kind(&tok,tok,TK_IDENT)){
    type = type_suffix(&tok,tok,type);

    *rest = tok;
    return type;
  }

  if(consume(&tok,tok,"(")){
    Token *nest_start = tok;
    Type *tmp = &(Type){};
    declarator(&tok,tok,tmp);
    expect(&tok,tok,")");

    type = type_suffix(&tok,tok,type);
    Token *type_end = tok;

    type = declarator(&tok,nest_start,type);

    *rest = type_end;
    return type;
  }

  *rest = tok;
  return NULL;
}

bool is_same_type(Type *a,Type *b){
  if(a->ty == INT && b->ty == INT)
    return true;
  if(a->ty != b->ty)
    return false;
  return is_same_type(a->ptr_to,b->ptr_to);
}

bool is_convertible(Type *a,Type *b){
  if(a->ty == INT && b->ty == INT)
    return true;
  if(a->ty == INT || b->ty == INT)
    return false;
  return is_same_type(a->ptr_to,b->ptr_to);
}

int type_size(Type *a){
  if(a->ty == INT)
    return 4;
  if(a->ty == ARRAY)
    return a->array_size * type_size(a->ptr_to);
  return 8;
}

int type_alignment(Type *a){
  if(a->ty == INT)
    return 4;
  if(a->ty == ARRAY)
    return type_alignment(a->ptr_to);
  return 8;
}
