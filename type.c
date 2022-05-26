#include "mycc.h"

Type *type_int = &(Type){.ty = INT};

Type *newtype_ptr(Type *base){
  Type *type = calloc(1,sizeof(Type));
  type->ty = PTR;
  type->ptr_to = base;
  return type;
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
