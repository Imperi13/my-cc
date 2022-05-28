#include "mycc.h"

Type *global_decl_specifier(Token **rest,Token *tok);
Type *local_decl_specifier(Token **rest,Token *tok);
Obj *type_suffix(Token **rest,Token *tok,Obj *type);
Obj *declarator(Token **rest,Token *tok,Obj *type);

Type *type_int = &(Type){.ty = INT};
Type *type_char = &(Type){.ty = CHAR};

Type *newtype_ptr(Type *base){
  Type *type = calloc(1,sizeof(Type));
  type->ty = PTR;
  type->ptr_to = base;
  return type;
}

Obj *parse_global_decl(Token **rest,Token *tok){
  Obj *obj = calloc(1,sizeof(Obj));
  if(!global_decl_specifier(&dummy_token,tok)){
    return NULL;
  }
  obj->type = global_decl_specifier(&tok,tok);

  obj = declarator(&tok,tok,obj);

  *rest = tok;
  return obj;
}

Obj *parse_local_decl(Token **rest,Token *tok){
  Obj *obj = calloc(1,sizeof(Obj));
  if(!local_decl_specifier(&dummy_token,tok)){
    return NULL;
  }
  obj->type = local_decl_specifier(&tok,tok);

  obj = declarator(&tok,tok,obj);

  *rest = tok;
  return obj;
}

Type *global_decl_specifier(Token **rest,Token *tok){
  if(!equal_kind(tok,TK_INT) && !equal_kind(tok,TK_CHAR))
    return type_int;

  if(consume_kind(&tok,tok,TK_INT)){
    *rest = tok;
    return type_int;
  }

  if(consume_kind(&tok,tok,TK_CHAR)){
    *rest = tok;
    return type_char;
  }

  *rest = tok;
  return NULL;
}

Type *local_decl_specifier(Token **rest,Token *tok){
  if(consume_kind(&tok,tok,TK_INT)){
    *rest = tok;
    return type_int;
  }

  if(consume_kind(&tok,tok,TK_CHAR)){
    *rest = tok;
    return type_char;
  }

  *rest = tok;
  return NULL;
}

Obj *type_suffix(Token **rest,Token *tok,Obj *obj){
  if(consume(&tok,tok,"(")){
    Type *func_type = calloc(1,sizeof(Type));
    func_type->ty = FUNC;
    func_type->return_type = obj->type;

    if(!consume(&tok,tok,")")){
      do{
        Obj *argtype = parse_global_decl(&tok,tok);

        TypeList *push_argtype = calloc(1,sizeof(TypeList));
        push_argtype->type = argtype->type;

        if(!obj->type->argtype_back){
          obj->type->argtype_front = push_argtype;
          obj->type->argtype_back = push_argtype;
        }else{
          obj->type->argtype_back->next = push_argtype;
          obj->type->argtype_back = push_argtype;
        }

        obj->type->arg_size++;
      }while(consume(&tok,tok,","));
      expect(&tok,tok,")");
    }

    obj->type = func_type;

    *rest = tok;
    return obj;
  }

  if(consume(&tok,tok,"[")){
    Type *array_type = calloc(1,sizeof(Type));
    array_type->ty = ARRAY;
    array_type->array_size = expect_number(&tok,tok);
    array_type->ptr_to = obj->type;

    expect(&tok,tok,"]");

    obj->type = array_type;

    *rest = tok;
    return obj;
  }

  *rest = tok;
  return obj;
}

Obj *declarator(Token **rest,Token *tok,Obj *obj){
  while(consume(&tok,tok,"*")){
    obj->type = newtype_ptr(obj->type);
  }


  if(equal_kind(tok,TK_IDENT)){
    Token *ident = consume_kind(&tok,tok,TK_IDENT);

    obj->name = ident->str;
    obj->len = ident->len;
    obj = type_suffix(&tok,tok,obj);

    *rest = tok;
    return obj;
  }

  if(consume(&tok,tok,"(")){
    Token *nest_start = tok;
    Obj *tmp = &(Obj){};
    declarator(&tok,tok,tmp);
    expect(&tok,tok,")");

    obj = type_suffix(&tok,tok,obj);
    Token *type_end = tok;

    obj = declarator(&tok,nest_start,obj);

    *rest = type_end;
    return obj;
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

bool is_numeric(Type *a){
  if(a->ty == INT || a->ty == CHAR)
    return true;
  return false;
}

bool is_convertible(Type *a,Type *b){
  if(is_numeric(a) && is_numeric(b))
    return true;
  if(a->ty == INT || b->ty == INT)
    return false;
  return is_same_type(a->ptr_to,b->ptr_to);
}

int type_size(Type *a){
  if(a->ty == CHAR)
    return 1;
  if(a->ty == INT)
    return 4;
  if(a->ty == ARRAY)
    return a->array_size * type_size(a->ptr_to);
  return 8;
}

int type_alignment(Type *a){
  if(a->ty == CHAR)
    return 1;
  if(a->ty == INT)
    return 4;
  if(a->ty == ARRAY)
    return type_alignment(a->ptr_to);
  return 8;
}
