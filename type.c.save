#include "mycc.h"

Type *decl_specifier(Token **rest, Token *tok, TypeQual *qual);
Obj *type_suffix(Token **rest, Token *tok, Obj *type);
Obj *declarator(Token **rest, Token *tok, Obj *type);

Type *abstract_declarator(Token **rest, Token *tok, Type *type);

StructDef *struct_defs;
EnumDef *enum_defs;
ConstList *enum_consts;

Type *type_void = &(Type){.ty = VOID};
Type *type_int = &(Type){.ty = INT};
Type *type_char = &(Type){.ty = CHAR};

Type *newtype_ptr(Type *base) {
  Type *type = calloc(1, sizeof(Type));
  type->ty = PTR;
  type->ptr_to = base;
  return type;
}

Type *newtype_struct(StructDef *st_def) {
  Type *type = calloc(1, sizeof(Type));
  type->ty = STRUCT;
  type->st = st_def;
  return type;
}

Type *newtype_enum(EnumDef *enum_def) {
  Type *type = calloc(1, sizeof(Type));
  type->ty = ENUM;
  type->en = enum_def;
  return type;
}

StructDef *find_struct(char *name, int len) {
  for (StructDef *now = struct_defs; now; now = now->next)
    if (now->len == len && !memcmp(name, now->name, now->len))
      return now;
  return NULL;
}

EnumDef *find_enum(char *name, int len) {
  for (EnumDef *now = enum_defs; now; now = now->next)
    if (now->len == len && !memcmp(name, now->name, now->len))
      return now;
  return NULL;
}

EnumConst *find_enum_const(char *name, int len) {
  for (ConstList *now = enum_consts; now; now = now->next)
    if (now->en_const->len == len &&
        !memcmp(name, now->en_const->name, now->en_const->len))
      return now->en_const;
  return NULL;
}

Member *find_member(StructDef *st, char *name, int len) {
  for (Member *now = st->members; now; now = now->next)
    if (now->len == len && !memcmp(name, now->name, now->len))
      return now;
  return NULL;
}

// parse_*_decl
// parse symbol&type& (argument symbol&type)
Obj *parse_global_decl(Token **rest, Token *tok) {
  Obj *obj = calloc(1, sizeof(Obj));
  TypeQual *qual = calloc(1, sizeof(TypeQual));
  obj->type = decl_specifier(&tok, tok, qual);

  if (obj->type == NULL)
    obj->type = type_int;

  if (consume(&tok, tok, ";")) {
    *rest = tok;
    return obj;
  }

  obj = declarator(&tok, tok, obj);

  obj->qual = qual;

  *rest = tok;
  return obj;
}

Obj *parse_local_decl(Token **rest, Token *tok) {
  Obj *obj = calloc(1, sizeof(Obj));
  TypeQual *qual = calloc(1, sizeof(TypeQual));
  if (!is_decl_spec(tok)) {
    return NULL;
  }
  obj->type = decl_specifier(&tok, tok, qual);

  if (consume(&tok, tok, ";")) {
    *rest = tok;
    return obj;
  }

  obj = declarator(&tok, tok, obj);

  if (consume(&tok, tok, "=")) {
    obj->init_var = assign(&tok, tok);
  }
  expect(&tok, tok, ";");

  *rest = tok;
  return obj;
}

Type *type_name(Token **rest, Token *tok) {
  TypeQual *qual = calloc(1, sizeof(TypeQual));
  Type *type = decl_specifier(&tok, tok, qual);
  if (!type) {
    *rest = tok;
    return NULL;
  }

  type = abstract_declarator(&tok, tok, type);
  *rest = tok;
  return type;
}

// abstract_declaratorとしてparseできないときはNULLを返す
Type *abstract_declarator(Token **rest, Token *tok, Type *type) {
  while (consume(&tok, tok, "*")) {
    type = newtype_ptr(type);
    consume_kind(&tok, tok, TK_CONST);
  }

  if (consume(&tok, tok, "(")) {
    Token *nest_start = tok;
    Type *tmp = &(Type){};
    if (!abstract_declarator(&tok, tok, tmp))
      return NULL;
    expect(&tok, tok, ")");

    Obj *obj = calloc(1, sizeof(Obj));
    obj->type = type;
    obj = type_suffix(&tok, tok, obj);
    Token *type_end = tok;

    type = abstract_declarator(&tok, nest_start, obj->type);

    *rest = type_end;
    return type;
  }

  if (equal_kind(tok, TK_IDENT))
    return NULL;

  Obj *obj = calloc(1, sizeof(Obj));
  obj->type = type;
  obj = type_suffix(&tok, tok, obj);

  *rest = tok;
  return obj->type;
}

Type *parse_struct(Token **rest, Token *tok) {
  expect_kind(&tok, tok, TK_STRUCT);
  Token *ty_name = consume_kind(&tok, tok, TK_IDENT);
  if (!ty_name)
    error_at(tok->str, "anonymous struct is not implemented");

  StructDef *st_def = find_struct(ty_name->str, ty_name->len);
  if (st_def && st_def->is_defined) {
    if (consume(&tok, tok, "{"))
      error_at(tok->str, "struct '%.*s' is already defined", ty_name->len,
               ty_name->str);
    *rest = tok;
    return newtype_struct(st_def);
  }

  if (!st_def) {
    st_def = calloc(1, sizeof(StructDef));

    st_def->next = struct_defs;
    struct_defs = st_def;
  }

  st_def->name = ty_name->str;
  st_def->len = ty_name->len;
  st_def->is_defined = false;
  st_def->size = 0;

  if (!consume(&tok, tok, "{")) {
    *rest = tok;
    return newtype_struct(st_def);
  }

  st_def->is_defined = true;

  while (!consume(&tok, tok, "}")) {
    Obj *obj = parse_local_decl(&tok, tok);
    Member *member = calloc(1, sizeof(Member));

    member->name = obj->name;
    member->len = obj->len;
    member->type = obj->type;
    member->offset =
        offset_alignment(st_def->size, 0, type_alignment(obj->type));

    st_def->size = member->offset + type_size(member->type);

    member->next = st_def->members;
    st_def->members = member;
  }

  *rest = tok;
  return newtype_struct(st_def);
}

Type *parse_enum(Token **rest, Token *tok) {
  expect_kind(&tok, tok, TK_ENUM);
  if (!equal_kind(tok, TK_IDENT))
    error_at(tok->str, "anonymous enum is not implemented");

  Token *ty_name = consume_kind(&tok, tok, TK_IDENT);

  EnumDef *en_def = find_enum(ty_name->str, ty_name->len);
  if (en_def && en_def->is_defined) {
    if (consume(&tok, tok, "{"))
      error_at(tok->str, "struct '%.*s' is already defined", ty_name->len,
               ty_name->str);

    *rest = tok;
    return newtype_enum(en_def);
  }

  if (!en_def) {
    en_def = calloc(1, sizeof(EnumDef));

    en_def->next = enum_defs;
    enum_defs = en_def;
  }

  en_def->name = ty_name->str;
  en_def->len = ty_name->len;
  en_def->is_defined = false;

  if (!consume(&tok, tok, "{")) {
    *rest = tok;
    return newtype_enum(en_def);
  }

  en_def->is_defined = true;

  int const_val = 0;
  while (!consume(&tok, tok, "}")) {
    Token *const_name = consume_kind(&tok, tok, TK_IDENT);
    EnumConst *en_const = calloc(1, sizeof(EnumConst));

    en_const->name = const_name->str;
    en_const->len = const_name->len;
    en_const->val = const_val;

    ConstList *push_const = calloc(1, sizeof(ConstList));
    push_const->en_const = en_const;

    push_const->next = en_def->enum_consts;
    en_def->enum_consts = push_const;

    push_const = calloc(1, sizeof(ConstList));
    push_const->en_const = en_const;

    push_const->next = enum_consts;
    enum_consts = push_const;

    const_val++;
    consume(&tok, tok, ",");
  }

  *rest = tok;
  return newtype_enum(en_def);
}

bool is_decl_spec(Token *tok) {
  return equal_kind(tok, TK_VOID) || equal_kind(tok, TK_INT) ||
         equal_kind(tok, TK_CHAR) || equal_kind(tok, TK_STRUCT) ||
         equal_kind(tok, TK_ENUM) || equal_kind(tok, TK_CONST) ||
         equal_kind(tok, TK_EXTERN) || equal_kind(tok, TK_STATIC);
}

Type *decl_specifier(Token **rest, Token *tok, TypeQual *qual) {
  Type *type = NULL;
  while (is_decl_spec(tok)) {
    if (consume_kind(&tok, tok, TK_CONST))
      qual->is_const = true;

    if (consume_kind(&tok, tok, TK_EXTERN))
      qual->is_extern = true;

    if (consume_kind(&tok, tok, TK_STATIC))
      qual->is_static = true;

    if (consume_kind(&tok, tok, TK_VOID)) {
      if (type)
        error_at(tok->str, "dup type");
      type = type_void;
    }

    if (consume_kind(&tok, tok, TK_INT)) {
      if (type)
        error_at(tok->str, "dup type");
      type = type_int;
    }

    if (consume_kind(&tok, tok, TK_CHAR)) {
      if (type)
        error_at(tok->str, "dup type");
      type = type_char;
    }

    if (equal_kind(tok, TK_STRUCT)) {
      if (type)
        error_at(tok->str, "dup type");
      type = parse_struct(&tok, tok);
    }

    if (equal_kind(tok, TK_ENUM)) {
      if (type)
        error_at(tok->str, "dup type");
      type = parse_enum(&tok, tok);
    }
  }

  *rest = tok;
  return type;
}

Obj *type_suffix(Token **rest, Token *tok, Obj *obj) {
  if (consume(&tok, tok, "(")) {
    Type *func_type = calloc(1, sizeof(Type));
    func_type->ty = FUNC;
    func_type->return_type = obj->type;

    obj->arg_front = NULL;
    obj->arg_back = NULL;
    obj->arg_size = 0;
    obj->local_scope = calloc(1, sizeof(VarScope));
    obj->stack_size = 0;

    if (func_type->return_type->ty == STRUCT &&
        type_size(func_type->return_type) > 16) {
      obj->stack_size = 8;
      obj->is_buf_return = true;
    }

    if (equal_kind(tok, TK_VOID) && equal(tok->next, ")")) {
      expect_kind(&tok, tok, TK_VOID);
      expect(&tok, tok, ")");
      obj->type = func_type;

      *rest = tok;
      return obj;
    }

    if (!consume(&tok, tok, ")")) {
      do {

        if (type_name(&dummy_token, tok)) {
          Type *arg_type = type_name(&tok, tok);

          TypeList *push_argtype = calloc(1, sizeof(TypeList));
          push_argtype->type = arg_type;

          if (!func_type->argtype_back) {
            func_type->argtype_front = push_argtype;
            func_type->argtype_back = push_argtype;
          } else {
            func_type->argtype_back->next = push_argtype;
            func_type->argtype_back = push_argtype;
          }

          Obj *arg = calloc(1, sizeof(Obj));
          arg->type = arg_type;

          arg->offset = offset_alignment(obj->stack_size, type_size(arg->type),
                                         type_alignment(arg->type));
          obj->stack_size = arg->offset;

          ObjList *push_lvar = calloc(1, sizeof(ObjList));
          push_lvar->obj = arg;
          push_lvar->next = obj->local_scope->locals;
          obj->local_scope->locals = push_lvar;

          func_type->arg_size++;
          obj->arg_size++;
          ObjList *push_arg = calloc(1, sizeof(ObjList));
          push_arg->obj = arg;
          if (!obj->arg_front) {
            obj->arg_front = push_arg;
            obj->arg_back = push_arg;
          } else {
            obj->arg_back->next = push_arg;
            obj->arg_back = push_arg;
          }

          consume(&tok, tok, ",");
          continue;
        }

        Obj *arg = parse_global_decl(&tok, tok);
        if (arg->type->ty == ARRAY)
          arg->type->ty = PTR;

        TypeList *push_argtype = calloc(1, sizeof(TypeList));
        push_argtype->type = arg->type;

        if (!func_type->argtype_back) {
          func_type->argtype_front = push_argtype;
          func_type->argtype_back = push_argtype;
        } else {
          func_type->argtype_back->next = push_argtype;
          func_type->argtype_back = push_argtype;
        }

        if (find_obj(obj->local_scope->locals, arg->name, arg->len))
          error_at(tok->str, "duplicate arguments");

        arg->offset = offset_alignment(obj->stack_size, type_size(arg->type),
                                       type_alignment(arg->type));
        obj->stack_size = arg->offset;

        ObjList *push_lvar = calloc(1, sizeof(ObjList));
        push_lvar->obj = arg;
        push_lvar->next = obj->local_scope->locals;
        obj->local_scope->locals = push_lvar;

        func_type->arg_size++;
        obj->arg_size++;
        ObjList *push_arg = calloc(1, sizeof(ObjList));
        push_arg->obj = arg;
        if (!obj->arg_front) {
          obj->arg_front = push_arg;
          obj->arg_back = push_arg;
        } else {
          obj->arg_back->next = push_arg;
          obj->arg_back = push_arg;
        }
      } while (consume(&tok, tok, ","));
      expect(&tok, tok, ")");
    }

    obj->type = func_type;

    *rest = tok;
    return obj;
  }

  if (consume(&tok, tok, "[")) {
    Type *array_type = calloc(1, sizeof(Type));
    array_type->ty = ARRAY;
    array_type->array_size = expect_number(&tok, tok);

    expect(&tok, tok, "]");
    obj = type_suffix(&tok, tok, obj);

    array_type->ptr_to = obj->type;
    obj->type = array_type;

    *rest = tok;
    return obj;
  }

  *rest = tok;
  return obj;
}

Obj *declarator(Token **rest, Token *tok, Obj *obj) {
  while (consume(&tok, tok, "*")) {
    obj->type = newtype_ptr(obj->type);
    while (consume_kind(&tok, tok, TK_CONST)) {
    }
  }

  if (equal_kind(tok, TK_IDENT)) {
    Token *ident = consume_kind(&tok, tok, TK_IDENT);

    obj->name = ident->str;
    obj->len = ident->len;
    obj = type_suffix(&tok, tok, obj);

    *rest = tok;
    return obj;
  }

  if (consume(&tok, tok, "(")) {
    Token *nest_start = tok;
    Obj *tmp = &(Obj){};
    declarator(&tok, tok, tmp);
    expect(&tok, tok, ")");

    obj = type_suffix(&tok, tok, obj);
    Token *type_end = tok;

    obj = declarator(&tok, nest_start, obj);

    *rest = type_end;
    return obj;
  }

  *rest = tok;
  return NULL;
}

bool is_complete(Type *a) {
  if (a->ty == VOID)
    return false;
  if (a->ty == STRUCT && !a->st->is_defined)
    return false;
  return true;
}

bool is_numeric(Type *a) {
  if (a->ty == INT || a->ty == CHAR || a->ty == ENUM)
    return true;
  return false;
}

bool is_primitive(Type *a) {
  if (a->ty == INT || a->ty == CHAR || a->ty == VOID)
    return true;
  return false;
}

bool is_void_ptr(Type *a) { return a->ty == PTR && a->ptr_to->ty == VOID; }

bool is_null_ptr(Node *a) { return is_constexpr(a) && eval_constexpr(a) == 0; }

bool is_same_type(Type *a, Type *b) {
  if (a->ty != b->ty)
    return false;
  if (is_primitive(a))
    return true;
  if (a->ty == ARRAY)
    return a->array_size == b->array_size && is_same_type(a->ptr_to, b->ptr_to);
  if (a->ty == STRUCT)
    return a->st == b->st;
  return is_same_type(a->ptr_to, b->ptr_to);
}

bool is_compatible(Type *a, Node *b) {
  if (is_numeric(a) && is_numeric(b->type))
    return true;
  if (a->ty == PTR && b->type->ty == PTR &&
      (a->ptr_to->ty == VOID || b->type->ptr_to->ty == VOID))
    return true;
  if (a->ty == PTR && is_null_ptr(b))
    return true;
  if (a->ty == PTR && b->type->ty == ARRAY)
    return a->ptr_to->ty == VOID || is_same_type(a->ptr_to, b->type->ptr_to);
  if (a->ty == STRUCT && b->type->ty == STRUCT)
    return a->st == b->type->st;
  if (a->ty == INT || b->type->ty == INT)
    return false;
  return is_same_type(a->ptr_to, b->type->ptr_to);
}

int type_size(Type *a) {
  if (!is_complete(a))
    error("this type is incomplete");
  if (a->ty == CHAR)
    return 1;
  if (a->ty == INT)
    return 4;
  if (a->ty == ARRAY)
    return a->array_size * type_size(a->ptr_to);
  if (a->ty == STRUCT)
    return offset_alignment(a->st->size, 0, type_alignment(a));
  return 8;
}

int type_alignment(Type *a) {
  if (a->ty == VOID)
    error("void alignment");
  if (a->ty == CHAR)
    return 1;
  if (a->ty == INT)
    return 4;
  if (a->ty == ARRAY)
    return type_alignment(a->ptr_to);
  if (a->ty == STRUCT) {
    int align = 0;
    for (Member *member = a->st->members; member; member = member->next)
      if (type_alignment(member->type) > align)
        align = type_alignment(member->type);
    return align;
  }
  return 8;
}
