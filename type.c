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

// parse_*_decl
// parse symbol&type& (argument symbol&type)
Obj *parse_global_decl(Token **rest,Token *tok){
  Obj *obj = calloc(1,sizeof(Obj));
  now_function = obj;
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

    obj->arg_front = NULL;
    obj->arg_back = NULL;
    obj->locals = NULL;
    obj->arg_size=0;

    if(!consume(&tok,tok,")")){
      do{
        Obj *arg = parse_global_decl(&tok,tok);

        TypeList *push_argtype = calloc(1,sizeof(TypeList));
        push_argtype->type = arg->type;

        if(!obj->type->argtype_back){
          obj->type->argtype_front = push_argtype;
          obj->type->argtype_back = push_argtype;
        }else{
          obj->type->argtype_back->next = push_argtype;
          obj->type->argtype_back = push_argtype;
        }

        if(find_obj(obj->locals,arg->name,arg->len))
          error_at(tok->str,"duplicate arguments");

        ObjList *push_lvar = calloc(1,sizeof(ObjList));
        push_lvar->obj = arg;
        push_lvar->next = obj->locals;
        if(obj->locals)
          arg->offset = offset_alignment(obj->locals->obj->offset,type_size(arg->type),type_alignment(arg->type));
        else
          arg->offset = offset_alignment(0,type_size(arg->type),type_alignment(arg->type));
        obj->locals = push_lvar;

        func_type->arg_size++;
        obj->arg_size++;
        ObjList *push_arg = calloc(1,sizeof(ObjList));
        push_arg->obj = arg;
        if(!obj->arg_front){
          obj->arg_front = push_arg;
          obj->arg_back = push_arg;
        }else{
          obj->arg_back->next = push_arg;
          obj->arg_back = push_arg;
        }

        if(obj->arg_size > 6)
          error_at(tok->str,"more than 6 args is not implemented");
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

bool is_numeric(Type *a){
  if(a->ty == INT || a->ty == CHAR)
    return true;
  return false;
}

bool is_primitive(Type *a){
  if(a->ty == INT || a->ty == CHAR)
    return true;
  return false;
}

bool is_same_type(Type *a,Type *b){
  if(is_primitive(a) && is_primitive(b))
    return a->ty == b->ty;
  return is_same_type(a->ptr_to,b->ptr_to);
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
